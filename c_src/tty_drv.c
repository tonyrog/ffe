/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2022. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
/*
 * Tty driver that reads one character at the time and provides a
 * smart line for output.
 */

#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <limits.h>
#include <locale.h>
#include <unistd.h>
#include <termios.h>
#include <wchar.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/uio.h>

// #define PRIMITIVE_UTF8_CHECK 1
#include <langinfo.h>

#include "erl_nif.h"
#include "erl_driver.h"


#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

#define FALSE 0

static int tty_init(void);
static ErlDrvData tty_start(ErlDrvPort, char*);

/* Termcap functions. */
int tgetent(char* bp, char *name);
int tgetnum(char* cap);
int tgetflag(char* cap);
char *tgetstr(char* cap, char** buf);
char *tgoto(char* cm, int col, int line);
int tputs(char* cp, int affcnt, int (*outc)(int c));

/* Terminal capabilities in which we are interested. */

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4
#define OP_PUTC_SYNC 5
/* Control op */
#define CTRL_OP_GET_WINSIZE 100
#define CTRL_OP_GET_UNICODE_STATE 101
#define CTRL_OP_SET_UNICODE_STATE 102

/* We use 1024 as the buf size as that was the default buf size of FILE streams
   on all platforms that I checked. */
#define TTY_BUFFSIZE 1024

/* 
 * Tags used in line buffer to show that these bytes represent special characters,
 * Max unicode is 0x0010ffff, so we have lots of place for meta tags... 
 */
#define CONTROL_TAG 0x10000000U /* Control character, value in first position */
#define ESCAPED_TAG 0x01000000U /* Escaped character, value in first position */
#define TAG_MASK    0xFF000000U

#define MAXSIZE (1 << 16)

#define COL(_l) ((_l) % ctx->cols)
#define LINE(_l) ((_l) / ctx->cols)

#define NL '\n'

/* Main interface functions. */
static void tty_stop(ErlDrvData);
static void tty_from_erlang(ErlDrvData, char*, ErlDrvSizeT);
static void tty_output_ready(ErlDrvData, ErlDrvEvent);
static void tty_flush_tty(ErlDrvData);
static void tty_input_ready(ErlDrvData, ErlDrvEvent);
static void tty_stop_select(ErlDrvEvent, void*);
static int16_t get_int16(char*);

// drv data
typedef struct {
    int fd;
    ErlDrvPort port;
    int send_ok;
    int terminate;
    ErlDrvBinary *putcbuf;
    int putcpos;
    int putclen;
    int lbuf_size;
    uint32_t *lbuf;	/* The current line buffer */
    int llen;		/* The current line length */
    int lpos;           /* The current "cursor position" in the line buffer */
    struct termios smode;
    struct termios rmode;

    char *capbuf;
    char *up, *down, *left, *right;
    int cols, xn;
    volatile int cols_needs_update;

    int utf8_mode;
    uint8_t utf8buf[4]; /* for incomplete input */
    int utf8buf_size; /* size of incomplete input */
} TTY_ctx_t;

static TTY_ctx_t* gctx = NULL;

static ErlDrvEntry tty_drv_entry;

/* Functions that work on the line buffer. */
static int start_lbuf(TTY_ctx_t* ctx);
static int stop_lbuf(TTY_ctx_t* ctx);
static int put_chars(TTY_ctx_t* ctx, uint8_t*,int);
static int move_rel(TTY_ctx_t* ctx, int);
static int ins_chars(TTY_ctx_t* ctx, uint8_t *,int);
static int del_chars(TTY_ctx_t* ctx, int);
static int step_over_chars(TTY_ctx_t* ctx, int);
static int insert_buf(TTY_ctx_t* ctx, uint8_t*,int);
static int write_buf(TTY_ctx_t* ctx, uint32_t *,int,int);
static int outc(int c);
static int move_cursor(TTY_ctx_t* ctx, int,int);
static int cp_pos_to_col(TTY_ctx_t* ctx, int cp_pos);

/* Termcap functions. */
static int start_termcap(TTY_ctx_t* ctx);
static int stop_termcap(TTY_ctx_t* ctx);
static int move_left(TTY_ctx_t* ctx,int);
static int move_right(TTY_ctx_t* ctx,int);
static int move_up(TTY_ctx_t* ctx,int);
static int move_down(TTY_ctx_t* ctx,int);
static void update_cols(TTY_ctx_t* ctx);

/* Terminal setting functions. */
static int tt_init(TTY_ctx_t* ctx, int,int,int);
static int tt_set(TTY_ctx_t* ctx);
static int tt_reset(TTY_ctx_t* ctx);

static ErlDrvSSizeT tty_control(ErlDrvData, unsigned int,
				  char *, ErlDrvSizeT, char **, ErlDrvSizeT);
static void cont(int);
static void winch(int);

//#define LOG_DEBUG

#ifdef LOG_DEBUG

#define DEBUGLOG(X) 				\
do {						\
	my_debug_printf X;    			\
} while (0)

static void my_debug_printf(char *fmt, ...)
{
    char buffer[1024];
    va_list args;

    va_start(args, fmt);
    enif_vsnprintf(buffer,1024,fmt,args);
    va_end(args);
    enif_fprintf(stderr,"%s\n",buffer);
}

#else

#define DEBUGLOG(X)

#endif


static int tty_init(void)
{
    return 0;
}

static ErlDrvData tty_start(ErlDrvPort port, char* buf)
{
    char *s, *t, *l;
    int canon, echo, sig;	/* Terminal characteristics */
    int flag;
    int fd = -1;
    extern int using_oldshell; /* set this to let the rest of erts know */
    TTY_ctx_t* ctx;
    
    DEBUGLOG(("tty_start: driver input \"%s\"", buf));

    ctx = malloc(sizeof(TTY_ctx_t));
    memset(ctx, 0, sizeof(TTY_ctx_t));
    
    ctx->fd   = -1;
    ctx->port = port;
    ctx->lbuf_size = BUFSIZ;

    /* Set the terminal modes to default leave as is. */
    canon = echo = sig = 0;

    /* Parse the input parameters. */
    for (s = strchr(buf, ' '); s; s = t) {
        s++;
        /* Find end of this argument (start of next) and insert NUL. */
        if ((t = strchr(s, ' '))) {
            *t = '\0';
        }
        if ((flag = ((*s == '+') ? 1 : ((*s == '-') ? -1 : 0)))) {
            if (s[1] == 'c') canon = flag;
            if (s[1] == 'e') echo = flag;
            if (s[1] == 's') sig = flag;
        }
        else if ((fd = open(s, O_RDWR, 0)) < 0) {
            DEBUGLOG(("tty_start: failed to open tty_fd, open(%s, O_RDWR, 0)) = %d\n", s, fd));
            return ERL_DRV_ERROR_GENERAL;
        }
    }

    ctx->fd = (fd < 0) ? 0 : fd;
    gctx = ctx;

    if (tt_init(ctx, canon, echo, sig) < 0 ||
        tt_set(ctx) < 0) {
        DEBUGLOG(("tty_start: failed init tty or set tty\n"));
	ctx->port = (ErlDrvPort)-1;
	tt_reset(ctx);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* Set up smart line and termcap stuff. */
    if (!start_lbuf(ctx) || !start_termcap(ctx)) {
        DEBUGLOG(("tty_start: failed to start_lbuf or start_termcap\n"));
	stop_lbuf(ctx);		/* Must free this */
	tt_reset(ctx);
	return ERL_DRV_ERROR_GENERAL;
    }

    // set non blocking
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK);


#ifdef PRIMITIVE_UTF8_CHECK
    setlocale(LC_CTYPE, "");  /* Set international environment, 
				 ignore result */
    if (((l = getenv("LC_ALL"))   && *l) ||
	((l = getenv("LC_CTYPE")) && *l) ||
	((l = getenv("LANG"))     && *l)) {
	if (strstr(l, "UTF-8"))
	    ctx->utf8_mode = 1;
    }
    
#else
    l = setlocale(LC_CTYPE, "");  /* Set international environment */
    if (l != NULL) {
	ctx->utf8_mode = (strcmp(nl_langinfo(CODESET), "UTF-8") == 0);
	DEBUGLOG(("tty_start: setlocale: %s",l));
    }
#endif
    DEBUGLOG(("tty_start: utf8_mode is %s",(ctx->utf8_mode) ? "on" : "off"));
    signal(SIGCONT, cont);
    signal(SIGWINCH, winch);

    driver_select(port, (ErlDrvEvent)(uintptr_t)ctx->fd, ERL_DRV_READ|ERL_DRV_USE, 1);
    ctx->port = port;

    /* we need to know this when we enter the break handler */
    using_oldshell = 0;

    DEBUGLOG(("tty_start: successful start\n"));
    return (ErlDrvData)ctx;
}


#define DEF_HEIGHT 24
#define DEF_WIDTH 80
static void tty_get_window_size(TTY_ctx_t* ctx,
				uint32_t *width, uint32_t *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(ctx->fd,TIOCGWINSZ,&ws) == 0) {
	*width = (uint32_t) ws.ws_col;
	*height = (uint32_t) ws.ws_row;
	if (*width <= 0) 
	    *width = DEF_WIDTH;
	if (*height <= 0) 
	    *height = DEF_HEIGHT;
	return;
    }
#endif
    *width = DEF_WIDTH;
    *height = DEF_HEIGHT;
}
    
static ErlDrvSSizeT tty_control(ErlDrvData drv_data,
				  unsigned int command,
				  char *buf, ErlDrvSizeT len,
				  char **rbuf, ErlDrvSizeT rlen)
{
    TTY_ctx_t* ctx = (TTY_ctx_t*) drv_data;    
    char resbuff[2*sizeof(uint32_t)];
    ErlDrvSizeT res_size;
    gctx = ctx;
    switch (command) {
    case CTRL_OP_GET_WINSIZE:
	{
	    uint32_t w,h;
	    tty_get_window_size(ctx, &w,&h);
	    memcpy(resbuff,&w,sizeof(uint32_t));
	    memcpy(resbuff+sizeof(uint32_t),&h,sizeof(uint32_t));
	    res_size = 2*sizeof(uint32_t);
	}
	break;
    case CTRL_OP_GET_UNICODE_STATE:
	*resbuff = (ctx->utf8_mode) ? 1 : 0;
	res_size = 1;
	break;
    case CTRL_OP_SET_UNICODE_STATE:
	if (len > 0) {
	    int m = (int) *buf;
	    *resbuff = (ctx->utf8_mode) ? 1 : 0;
	    res_size = 1;
	    ctx->utf8_mode = (m) ? 1 : 0;
	} else {
	    return 0;
	}
	break;
    default:
	return -1;
    }
    if (rlen < res_size) {
	*rbuf = driver_alloc(res_size);
    }
    memcpy(*rbuf,resbuff,res_size);
    return res_size;
}


static void tty_stop(ErlDrvData tty_data)
{
    TTY_ctx_t* ctx = (TTY_ctx_t*) tty_data;
    
    DEBUGLOG(("tty_stop: port = %d\n", ctx->port));
    if (gctx == ctx)
	gctx = NULL;
    if (ctx->port != (ErlDrvPort)-1) {
	stop_lbuf(ctx);
	stop_termcap(ctx);
	tt_reset(ctx);
	driver_select(ctx->port, (ErlDrvEvent)(uintptr_t)ctx->fd,
                      ERL_DRV_WRITE|ERL_DRV_READ|ERL_DRV_USE, 0);
	signal(SIGCONT, SIG_DFL);
	signal(SIGWINCH, SIG_DFL);
    }
    ctx->port = (ErlDrvPort)-1;
    ctx->fd = -1;
    ctx->terminate = 0;
}

static int put_utf8(int ch, uint8_t *target, int sz, int *pos)
{
    uint32_t x = (uint32_t) ch;
    if (x < 0x80) {
    if (*pos >= sz) {
	return -1;
    }
	target[(*pos)++] = (uint8_t) x;
    }
    else if (x < 0x800) {
	if (((*pos) + 1) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((uint8_t) (x >> 6)) | 
			    ((uint8_t) 0xC0));
	target[(*pos)++] = (((uint8_t) (x & 0x3F)) | 
			    ((uint8_t) 0x80));
    } else if (x < 0x10000) {
	if ((x >= 0xD800 && x <= 0xDFFF) ||
	    (x == 0xFFFE) ||
	    (x == 0xFFFF)) { /* Invalid unicode range */
	    return -1;
	}
	if (((*pos) + 2) >= sz) {
	    return -1;
	}

	target[(*pos)++] = (((uint8_t) (x >> 12)) | 
			    ((uint8_t) 0xE0));
	target[(*pos)++] = ((((uint8_t) (x >> 6)) & 0x3F)  | 
			    ((uint8_t) 0x80));
	target[(*pos)++] = (((uint8_t) (x & 0x3F)) | 
			    ((uint8_t) 0x80));
    } else if (x < 0x110000) { /* Standard imposed max */
	if (((*pos) + 3) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((uint8_t) (x >> 18)) | 
			    ((uint8_t) 0xF0));
	target[(*pos)++] = ((((uint8_t) (x >> 12)) & 0x3F)  | 
			    ((uint8_t) 0x80));
	target[(*pos)++] = ((((uint8_t) (x >> 6)) & 0x3F)  | 
			    ((uint8_t) 0x80));
	target[(*pos)++] = (((uint8_t) (x & 0x3F)) | 
			    ((uint8_t) 0x80));
    } else {
	return -1;
    }
    return 0;
}
    

static int pick_utf8(uint8_t *s, int sz, int *pos) 
{
    int size = sz - (*pos);
    uint8_t *source;
    uint32_t unipoint;

    if (size > 0) {
	source = s + (*pos);
	if (((*source) & ((uint8_t) 0x80)) == 0) {
	    unipoint = (int) *source;
	    ++(*pos);
	    return (int) unipoint;
	} else if (((*source) & ((uint8_t) 0xE0)) == 0xC0) {
	    if (size < 2) {
		return -2;
	    }
	    if (((source[1] & ((uint8_t) 0xC0)) != 0x80) ||
		((*source) < 0xC2) /* overlong */) {
		return -1;
	    }
	    (*pos) += 2;
	    unipoint = 
		(((uint32_t) ((*source) & ((uint8_t) 0x1F))) << 6) |
		((uint32_t) (source[1] & ((uint8_t) 0x3F))); 	
	    return (int) unipoint;
	} else if (((*source) & ((uint8_t) 0xF0)) == 0xE0) {
	    if (size < 3) {
		return -2;
	    }
	    if (((source[1] & ((uint8_t) 0xC0)) != 0x80) ||
		((source[2] & ((uint8_t) 0xC0)) != 0x80) ||
		(((*source) == 0xE0) && (source[1] < 0xA0)) /* overlong */ ) {
		return -1;
	    }
	    if ((((*source) & ((uint8_t) 0xF)) == 0xD) && 
		((source[1] & 0x20) != 0)) {
		return -1;
	    }
	    if (((*source) == 0xEF) && (source[1] == 0xBF) &&
		((source[2] == 0xBE) || (source[2] == 0xBF))) {
		return -1;
	    }
	    (*pos) += 3;
	    unipoint = 
		(((uint32_t) ((*source) & ((uint8_t) 0xF))) << 12) |
		(((uint32_t) (source[1] & ((uint8_t) 0x3F))) << 6) |
		((uint32_t) (source[2] & ((uint8_t) 0x3F))); 	 	
	    return (int) unipoint;
	} else if (((*source) & ((uint8_t) 0xF8)) == 0xF0) {
	    if (size < 4) {
		return -2 ;
	    }
	    if (((source[1] & ((uint8_t) 0xC0)) != 0x80) ||
		((source[2] & ((uint8_t) 0xC0)) != 0x80) ||
		((source[3] & ((uint8_t) 0xC0)) != 0x80) ||
		(((*source) == 0xF0) && (source[1] < 0x90)) /* overlong */) {
		return -1;
	    }
	    if ((((*source) & ((uint8_t)0x7)) > 0x4U) ||
		((((*source) & ((uint8_t)0x7)) == 0x4U) && 
		 ((source[1] & ((uint8_t)0x3F)) > 0xFU))) {
		return -1;
	    }
	    (*pos) += 4;
	    unipoint = 
		(((uint32_t) ((*source) & ((uint8_t) 0x7))) << 18) |
		(((uint32_t) (source[1] & ((uint8_t) 0x3F))) << 12) |
		(((uint32_t) (source[2] & ((uint8_t) 0x3F))) << 6) |
		((uint32_t) (source[3] & ((uint8_t) 0x3F))); 	 	
	    return (int) unipoint;
	} else {
	    return -1;
	}
    } else {
	return -1;
    }
}

static int octal_or_hex_positions(uint32_t c) 
{
    int x = 0;
    uint32_t ch = c;
    if (!ch) {
	return 1;
    }
    while(ch) {
	++x;
	ch >>= 3;
    }
    if (x <= 3) {
	return 3;
    }
    /* \x{H ...} format when larger than \777 */
    x = 0;
    ch = c;
    while(ch) {
	++x;
	ch >>= 4;
    }
    return x+3;
}

static void octal_or_hex_format(uint32_t ch, uint8_t *buf, int *pos)
{
    static uint8_t hex_chars[] = { '0','1','2','3','4','5','6','7','8','9',
				'A','B','C','D','E','F'};
    int num = octal_or_hex_positions(ch);
    if (num != 3) {
        // ASSERT(num > 3);
	buf[(*pos)++] = 'x';
	buf[(*pos)++] = '{';
	num -= 3;
	while(num--) {
	    buf[(*pos)++] = hex_chars[((ch >> (4*num)) & 0xFU)];
	}
	buf[(*pos)++] = '}';
    } else {
	while(num--) {
	    buf[(*pos)++] = ((uint8_t) ((ch >> (3*num)) & 0x7U) + '0');
	}
    }	
}

/*
 * Check that there is enough room in all buffers to copy all pad chars
 * and stiff we need If not, realloc lbuf.
 */
static int check_buf_size(TTY_ctx_t* ctx, uint8_t *s, int n)
{
    int pos = 0;
    int ch;
    int size = 10;

    DEBUGLOG(("check_buf_size: n = %d",n));
    while(pos < n) {
	/* Indata is always UTF-8 */
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("check_buf_size: Invalid UTF8:%d",ch));
	    ++pos;
	} 
	if (ctx->utf8_mode) { /* That is, terminal is UTF8 compliant */
	    if (ch >= 128 || isprint(ch)) {
#ifdef HAVE_WCWIDTH
		int width;
#endif
		DEBUGLOG(("check_buf_size: Printable(UTF-8:%d):%d",pos,ch));
		size++;
#ifdef HAVE_WCWIDTH
		if ((width = wcwidth(ch)) > 1) {
		    size += width - 1;
		}
#endif
	    } else if (ch == '\t') {
		size += 8;
	    } else {
		DEBUGLOG(("check_buf_size: Magic(UTF-8:%d):%d",pos,ch));
		size += 2;
	    }
	} else {
	    if (ch <= 255 && isprint(ch)) {
		DEBUGLOG(("check_buf_size: Printable:%d",ch));
		size++;
	    } else if (ch == '\t') 
		size += 8;
	    else if (ch >= 128) {
		DEBUGLOG(("check_buf_size: Non printable:%d",ch));
		size += (octal_or_hex_positions(ch) + 1);
	    }
	    else {
		DEBUGLOG(("check_buf_size: Magic:%d",ch));
		size += 2;
	    }
	}
    }
		
    if (size + ctx->lpos >= ctx->lbuf_size) {

	ctx->lbuf_size = size + ctx->lpos + BUFSIZ;
	if ((ctx->lbuf = driver_realloc(ctx->lbuf, ctx->lbuf_size * sizeof(uint32_t))) == NULL) {
            DEBUGLOG(("check_buf_size: alloc failure of %d uint8_ts", ctx->lbuf_size * sizeof(uint32_t)));
	    driver_failure(ctx->port, -1);
	    return(0);
	}
    }
    DEBUGLOG(("check_buf_size: success\n"));
    return(1);
}


static void tty_from_erlang(ErlDrvData tty_data, char* buf, ErlDrvSizeT count)
{
    TTY_ctx_t* ctx = (TTY_ctx_t*) tty_data;
    ErlDrvSizeT sz = driver_sizeq(ctx->port);  

    ctx->putclen = count > TTY_BUFFSIZE ? TTY_BUFFSIZE : count;
    ctx->putcbuf = driver_alloc_binary(ctx->putclen);
    ctx->putcpos = 0;

    if (ctx->lpos > MAXSIZE) 
	put_chars(ctx, (uint8_t*)"\n", 1);

    DEBUGLOG(("tty_from_erlang: OP = %d", buf[0]));
    gctx = ctx;
    
    switch (buf[0]) {
    case OP_PUTC_SYNC:
        /* Using sync means that we have to send an ok to the
           controlling process for each command call. We delay
           sending ok if the driver queue exceeds a certain size.
           We do not set ourselves as a busy port, as this
           could be very bad for user_drv, if it gets blocked on
           the port_command. */
        /* fall through */
    case OP_PUTC:
	DEBUGLOG(("tty_from_erlang: OP: Putc(%lu)",(unsigned long) count-1));
	if (check_buf_size(ctx, (uint8_t*)buf+1, count-1) == 0)
	    return; 
	put_chars(ctx, (uint8_t*)buf+1, count-1);
	break;
    case OP_MOVE:
	move_rel(ctx, get_int16(buf+1));
	break;
    case OP_INSC:
	if (check_buf_size(ctx, (uint8_t*)buf+1, count-1) == 0)
	    return;
	ins_chars(ctx, (uint8_t*)buf+1, count-1);
	break;
    case OP_DELC:
	del_chars(ctx, get_int16(buf+1));
	break;
    case OP_BEEP:
	outc('\007');
	break;
    default:
	/* Unknown op, just ignore. */
	break;
    }

    driver_enq_bin(ctx->port,ctx->putcbuf,0,ctx->putcpos);
    driver_free_binary(ctx->putcbuf);

    if (sz == 0) {
        for (;;) {
            int written, qlen;
            SysIOVec *iov;

            iov = driver_peekq(ctx->port,&qlen);
            if (iov)
                written = writev(ctx->fd, (const struct iovec*)iov, qlen > MAXIOV ? MAXIOV : qlen);
            else
                written = 0;
            if (written < 0) {
                if (errno == EAGAIN || errno == EINTR) {
                    driver_select(ctx->port,(ErlDrvEvent)(long)ctx->fd,
                                  ERL_DRV_USE|ERL_DRV_WRITE,1);
                    break;
                } else {
                    DEBUGLOG(("tty_from_erlang: driver failure in writev(%d,..) = %d (errno = %d)\n", ctx->fd, written, errno));
		    driver_failure_posix(ctx->port, errno);
		    return;
                }
            } else {
                if (driver_deq(ctx->port, written) == 0)
                    break;
            }
        }
    }

    if (buf[0] == OP_PUTC_SYNC) {
        if (driver_sizeq(ctx->port) > TTY_BUFFSIZE && !ctx->terminate) {
            /* We delay sending the ack until the buffer has been consumed */
            ctx->send_ok = 1;
        } else {
            ErlDrvTermData spec[] = {
                ERL_DRV_PORT, driver_mk_port(ctx->port),
                ERL_DRV_ATOM, driver_mk_atom("ok"),
                ERL_DRV_TUPLE, 2
            };
            erl_drv_output_term(driver_mk_port(ctx->port), spec,
                                sizeof(spec) / sizeof(spec[0]));
        }
    }

}

static void tty_output_ready(ErlDrvData tty_data, ErlDrvEvent fd) {
    TTY_ctx_t* ctx = (TTY_ctx_t*) tty_data;
    gctx = ctx;
    for (;;) {
        int written, qlen;
        SysIOVec *iov;
        ErlDrvSizeT sz;

        iov = driver_peekq(ctx->port,&qlen);
        
        DEBUGLOG(("tty_to_tty: qlen = %d", qlen));

        if (iov)
            written = writev(ctx->fd, (const struct iovec*) iov, qlen > MAXIOV ? MAXIOV : qlen);
        else
            written = 0;
        if (written < 0) {
            if (errno == EINTR) {
	        continue;
	    } else if (errno != EAGAIN){
                DEBUGLOG(("tty_to_tty: driver failure in writev(%d,..) = %d (errno = %d)\n", ctx->fd, written, errno));
	        driver_failure_posix(ctx->port, errno);
            }
            break;
        } else {
            sz = driver_deq(ctx->port, written);
            if (sz < TTY_BUFFSIZE && ctx->send_ok) {
                ErlDrvTermData spec[] = {
                    ERL_DRV_PORT, driver_mk_port(ctx->port),
                    ERL_DRV_ATOM, driver_mk_atom("ok"),
                    ERL_DRV_TUPLE, 2
                };
                ctx->send_ok = 0;
                erl_drv_output_term(driver_mk_port(ctx->port), spec,
                                    sizeof(spec) / sizeof(spec[0]));
            }
            if (sz == 0) {
                driver_select(ctx->port,(ErlDrvEvent)(intptr_t)ctx->fd,
                              ERL_DRV_WRITE,0);
                if (ctx->terminate) {
                    /* flush has been called, which means we should terminate
                       when queue is empty. This will not send any exit
                       message */
                    DEBUGLOG(("tty_to_tty: tty_terminate normal\n"));
                    driver_failure_atom(ctx->port, "normal");
		}
                break;
            }
        }
    }

    return;
}

static void tty_flush_tty(ErlDrvData tty_data) {
    TTY_ctx_t* ctx = (TTY_ctx_t*) tty_data;
    gctx = ctx;    
    DEBUGLOG(("tty_flush_tty: .."));
    ctx->terminate = 1;
    return;
}

static void tty_input_ready(ErlDrvData tty_data, ErlDrvEvent fd)
{
    uint8_t b[1024];
    ssize_t i;
    int ch = 0, pos = 0;
    int left = 1024;
    uint8_t *p = b;
    uint8_t t[1024];
    int tpos;
    TTY_ctx_t* ctx = (TTY_ctx_t*) tty_data;
    gctx = ctx;
    
    if (ctx->utf8buf_size > 0) {
	memcpy(b,ctx->utf8buf,ctx->utf8buf_size);
	left -= ctx->utf8buf_size;
	p += ctx->utf8buf_size;
	ctx->utf8buf_size = 0;
    }

    DEBUGLOG(("tty_from_tty: remainder = %d", left));
    
    if ((i = read((int)(intptr_t)fd, (char *) p, left)) >= 0) {
	if (p != b) {
	    i += (p - b);
	}
	if (ctx->utf8_mode) { /* Hopefully an UTF8 terminal */
	    while(pos < i && (ch = pick_utf8(b,i,&pos)) >= 0)
		;
	    if (ch == -2 && i - pos <= 4) {
		/* uint8_ts left to care for */
		ctx->utf8buf_size = i -pos;
		memcpy(ctx->utf8buf,b+pos,ctx->utf8buf_size);
	    } else if (ch == -1) {
		DEBUGLOG(("tty_from_tty: Giving up on UTF8 mode, invalid character"));
		ctx->utf8_mode = 0;
		goto latin_terminal;
	    }
	    driver_output(ctx->port, (char *) b, pos);
	} else {
	latin_terminal:
	    tpos = 0;
	    while (pos < i) {
		while (tpos < 1020 && pos < i) { /* Max 4 uint8_ts for UTF8 */
		    put_utf8((int) b[pos++], t, 1024, &tpos);
		}
		driver_output(ctx->port, (char *) t, tpos);
		tpos = 0;
	    }
	}
    } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
        DEBUGLOG(("tty_from_tty: driver failure in read(%d,..) = %d (errno = %d)\n", (int)(intptr_t)fd, i, errno));
	driver_failure(ctx->port, -1);
    }
}

static void tty_stop_select(ErlDrvEvent e, void* _)
{
    int fd = (int)(long)e;
    if (fd != 0) {
	close(fd);
    }
}

/* Procedures for putting and getting integers to/from strings. */
static int16_t get_int16(char *s)
{
    return ((*s << 8) | ((uint8_t*)s)[1]);
}

static int start_lbuf(TTY_ctx_t* ctx)
{
    if (!ctx->lbuf &&
	!(ctx->lbuf = (uint32_t*) driver_alloc(ctx->lbuf_size * sizeof(uint32_t))))
      return 0;
    ctx->llen = 0;
    ctx->lpos = 0;
    return 1;
}

static int stop_lbuf(TTY_ctx_t* ctx)
{
    if (ctx->lbuf) {
	driver_free(ctx->lbuf);
	ctx->lbuf = NULL;
    }
    return 1;
}

/* Put l uint8_ts (in UTF8) from s into the buffer and output them. */
static int put_chars(TTY_ctx_t* ctx, uint8_t *s, int l)
{
    int n;

    n = insert_buf(ctx, s, l);
    if (ctx->lpos > ctx->llen)
        ctx->llen = ctx->lpos;
    if (n > 0)
        write_buf(ctx, ctx->lbuf + ctx->lpos - n, n, 0);
    return 1;
}

/*
 * Move the current position forwards or backwards within the current
 * line. We know about padding.
 */
static int move_rel(TTY_ctx_t* ctx, int n)
{
    int npos;			/* The new position */

    /* Step forwards or backwards over the buffer. */
    npos = step_over_chars(ctx, n);

    /* Calculate move, updates pointers and move the cursor. */
    move_cursor(ctx, ctx->lpos, npos);
    ctx->lpos = npos;
    return 1;
}

/* Insert characters into the buffer at the current position. */
static int ins_chars(TTY_ctx_t* ctx, uint8_t *s, int l)
{
    int n, tl;
    uint32_t *tbuf = NULL;    /* Suppress warning about use-before-set */

    /* Move tail of buffer to make space. */
    if ((tl = ctx->llen - ctx->lpos) > 0) {
	if ((tbuf = driver_alloc(tl * sizeof(uint32_t))) == NULL)
	    return 0;
	memcpy(tbuf, ctx->lbuf + ctx->lpos, tl * sizeof(uint32_t));
    }
    n = insert_buf(ctx, s, l);
    if (tl > 0) {
	memcpy(ctx->lbuf + ctx->lpos, tbuf, tl * sizeof(uint32_t));
	driver_free(tbuf);
    }
    ctx->llen += n;
    write_buf(ctx, ctx->lbuf + (ctx->lpos - n), ctx->llen - (ctx->lpos - n), 0);
    move_cursor(ctx, ctx->llen, ctx->lpos);
    return 1;
}

/*
 * Delete characters in the buffer. Can delete characters before (n < 0)
 * and after (n > 0) the current position. Cursor left at beginning of
 * deleted block.
 */
static int del_chars(TTY_ctx_t* ctx, int n)
{
    int i, l, r;
    int pos;
    int gcs; /* deleted grapheme characters */

    gctx = ctx;
    update_cols(ctx);
    

    /* Step forward or backwards over n logical characters. */
    pos = step_over_chars(ctx, n);
    DEBUGLOG(("del_chars: %d from %d %d %d\n", n, ctx->lpos, pos, ctx->llen));
    if (pos > ctx->lpos) {
	l = pos - ctx->lpos;		/* Buffer characters to delete */
	r = ctx->llen - ctx->lpos - l;	/* Characters after deleted */
        gcs = cp_pos_to_col(ctx,pos) - cp_pos_to_col(ctx,ctx->lpos);
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memmove(ctx->lbuf + ctx->lpos, ctx->lbuf + pos, r * sizeof(uint32_t));
	ctx->llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(ctx, ctx->lbuf + ctx->lpos, r, 0);
	for (i = gcs ; i > 0; --i)
	    outc(' ');
	if (ctx->xn && COL(cp_pos_to_col(ctx,ctx->llen)+gcs) == 0)
	{
	    outc(' ');
	    move_left(ctx, 1);
	}
	move_cursor(ctx, ctx->llen + gcs, ctx->lpos);
    }
    else if (pos < ctx->lpos) {
	l = ctx->lpos - pos;		/* Buffer characters */
	r = ctx->llen - ctx->lpos;	/* Characters after deleted */
	gcs = -move_cursor(ctx, ctx->lpos, ctx->lpos-l);	/* Move back */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memmove(ctx->lbuf + pos, ctx->lbuf + ctx->lpos, r * sizeof(uint32_t));
	ctx->lpos -= l;
	ctx->llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(ctx, ctx->lbuf + ctx->lpos, r, 0);
	for (i = gcs ; i > 0; --i)
	    outc(' ');
        if (ctx->xn && COL(cp_pos_to_col(ctx, ctx->llen)+gcs) == 0)
	{
	    outc(' ');
	    move_left(ctx,1);
	}
        move_cursor(ctx, ctx->llen + gcs, ctx->lpos);
    }
    return 1;
}

/* Step over n logical characters, check for overflow. */
static int step_over_chars(TTY_ctx_t* ctx, int n)
{
    uint32_t *c, *beg, *end;

    beg = ctx->lbuf;
    end = ctx->lbuf + ctx->llen;
    c = ctx->lbuf + ctx->lpos;
    for ( ; n > 0 && c < end; --n) {
	c++;
	while (c < end && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    c++;
    }
    for ( ; n < 0 && c > beg; n++) {
	--c;
	while (c > beg && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    --c;
    }
    return c - ctx->lbuf;
}

/*
 * Insert n characters into the buffer at lpos.
 * Know about pad characters and treat \n specially.
 */

static int insert_buf(TTY_ctx_t* ctx, uint8_t *s, int n)
{
    int pos = 0;
    int buffpos = ctx->lpos;
    int ch;

    while (pos < n) {
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("insert_buf: Invalid UTF8:%d",ch));
	    ++pos;
	}
	if ((ctx->utf8_mode && (ch >= 128 || isprint(ch))) || (ch <= 255 && isprint(ch))) {
	    DEBUGLOG(("insert_buf: Printable(UTF-8):%d",ch));
	    ctx->lbuf[ctx->lpos++] = (uint32_t) ch;
	} else if (ch >= 128) { /* not utf8 mode */
	    int nc = octal_or_hex_positions(ch);
	    ctx->lbuf[ctx->lpos++] = ((uint32_t) ch) | ESCAPED_TAG;
	    while (nc--) {
		ctx->lbuf[ctx->lpos++] = ESCAPED_TAG;
	    }
	} else if (ch == '\t') {
	    do {
		ctx->lbuf[ctx->lpos++] = (CONTROL_TAG | ((uint32_t) ch));
		ch = 0;
	    } while (ctx->lpos % 8);
	} else if (ch == '\e') {
	    DEBUGLOG(("insert_buf: ANSI Escape: \\e"));
	    ctx->lbuf[ctx->lpos++] = (CONTROL_TAG | ((uint32_t) ch));
	} else if (ch == '\n' || ch == '\r') {
	    write_buf(ctx, ctx->lbuf + buffpos, ctx->lpos - buffpos, 1);
	    outc('\r');
	    if (ch == '\n')
		outc('\n');
	    if (ctx->llen > ctx->lpos) {
		memmove(ctx->lbuf, ctx->lbuf + ctx->lpos, ctx->llen-ctx->lpos);
	    }
	    ctx->llen -= ctx->lpos;
	    ctx->lpos = buffpos = 0;
	} else {
	    DEBUGLOG(("insert_buf: Magic(UTF-8):%d",ch));
	    ctx->lbuf[ctx->lpos++] = ch | CONTROL_TAG;
	    ctx->lbuf[ctx->lpos++] = CONTROL_TAG;
	}
    }
    return ctx->lpos - buffpos; /* characters "written" into 
			      current buffer (may be less due to newline) */
}
	


/*
 * Write n characters in line buffer starting at s. Be smart about
 * non-printables. Know about pad characters and that \n can never
 * occur normally.
 */

static int write_buf(TTY_ctx_t* ctx, uint32_t *s, int n, int next_char_is_crnl)
{
    uint8_t ubuf[4];
    int uuint8_ts = 0, i;
    uint8_t lastput = ' ';

    gctx = ctx;
    update_cols(ctx);

    DEBUGLOG(("write_buf(%d, %d)",n,next_char_is_crnl));

    while (n > 0) {
	if (!(*s & TAG_MASK) ) {
	    if (ctx->utf8_mode) {
		uuint8_ts = 0;
		if (put_utf8((int) *s, ubuf, 4, &uuint8_ts) == 0) {
		    for (i = 0; i < uuint8_ts; ++i) {
			outc(ubuf[i]);
		    }
		    lastput = 0; /* Means the last written character was multiuint8_t UTF8 */
		}
	    } else {
		outc((uint8_t) *s);
		lastput = (uint8_t) *s;
	    }
	    --n;
	    ++s;
	} else if (*s == (CONTROL_TAG | ((uint32_t) '\t'))) {
	    outc(lastput = ' ');
	    --n; s++;
	    while (n > 0 && *s == CONTROL_TAG) {
		outc(lastput = ' ');
		--n; s++;
	    }
	} else if (*s == (CONTROL_TAG | ((uint32_t) '\e'))) {
	    outc(lastput = '\e');
	    --n;
	    ++s;
	} else if (*s & CONTROL_TAG) {
	    outc('^');
	    outc(lastput = ((uint8_t) ((*s == 0177) ? '?' : *s | 0x40)));
	    n -= 2;
	    s += 2;
	} else if (*s & ESCAPED_TAG) {
	    uint32_t ch = *s & ~(TAG_MASK);
	    uint8_t *octbuff;
	    uint8_t octtmp[256];
	    int octuint8_ts;
	    DEBUGLOG(("write_buf: Escaped: %d", ch));
	    octuint8_ts = octal_or_hex_positions(ch);
	    if (octuint8_ts > 256) {
		octbuff = driver_alloc(octuint8_ts);
	    } else {
		octbuff = octtmp;
	    }
	    octuint8_ts = 0;
	    octal_or_hex_format(ch, octbuff, &octuint8_ts);
            DEBUGLOG(("write_buf: octuint8_ts: %d", octuint8_ts));
	    outc('\\');
	    for (i = 0; i < octuint8_ts; ++i) {
		outc(lastput = octbuff[i]);
		DEBUGLOG(("write_buf: outc: %d", (int) lastput));
	    }
	    n -= octuint8_ts+1;
	    s += octuint8_ts+1;
	    if (octbuff != octtmp) {
		driver_free(octbuff);
	    }
	} else {
	    DEBUGLOG(("write_buf: Very unexpected character %d",(int) *s));
	    ++n;
	    --s;
	}
    }
    /* Check landed in first column of new line and have 'xn' bug.
     *   https://www.gnu.org/software/termutils/manual/termcap-1.3/html_node/termcap_27.html
     *
     * The 'xn' bugs (from what I understand) is that the terminal cursor does
     * not wrap to the next line when the current line is full. For example:
     *
     * If the terminal column size is 20 and we output 20 'a' the cursor will be
     * on row 1, column 21. While we actually want it at row 2 column 0. So to
     * achieve this the code below emits " \b", which will move the cursor to the
     * correct place.
     *
     * We should not apply this 'xn' workaround if we know that the next character
     * to be emitted is a cr|nl as that will wrap by itself.
     */
    n = s - ctx->lbuf;
    if (!next_char_is_crnl && ctx->xn && n != 0 && COL(cp_pos_to_col(ctx,n)) == 0) {
	if (n >= ctx->llen) {
	    outc(' ');
	} else if (lastput == 0) { /* A multiuint8_t UTF8 character */
	    for (i = 0; i < uuint8_ts; ++i) {
		outc(ubuf[i]);
	    }
	} else {
	    outc(lastput);
	}
	move_left(ctx, 1);
    }
    return 1;
}


/* The basic procedure for outputting one character. */
static int outc(int c)
{
    gctx->putcbuf->orig_bytes[gctx->putcpos++] = c;
    if (gctx->putcpos == gctx->putclen) {
        driver_enq_bin(gctx->port,gctx->putcbuf,0,gctx->putclen);
        driver_free_binary(gctx->putcbuf);
        gctx->putcpos = 0;
        gctx->putclen = TTY_BUFFSIZE;
        gctx->putcbuf = driver_alloc_binary(BUFSIZ);
    }
    return 1;
}

static int move_cursor(TTY_ctx_t* ctx, int from_pos, int to_pos)
{
    int from_col, to_col;
    int dc, dl;
    update_cols(ctx);

    from_col = cp_pos_to_col(ctx, from_pos);
    to_col = cp_pos_to_col(ctx, to_pos);

    dc = COL(to_col) - COL(from_col);
    dl = LINE(to_col) - LINE(from_col);
    DEBUGLOG(("move_cursor: from %d %d to %d %d => %d %d\n",
              from_pos, from_col, to_pos, to_col, dl, dc));
    if (dl > 0)
	move_down(ctx, dl);
    else if (dl < 0)
	move_up(ctx, -dl);
    if (dc > 0)
	move_right(ctx, dc);
    else if (dc < 0)
	move_left(ctx, -dc);
    return to_col-from_col;
}

/*
 * Returns the length of an ANSI escape code in a buffer, this function only consider
 * color escape sequences like `\e[33m` or `\e[21;33m`. If a sequence has no valid
 * terminator, the length is equal the number of characters between `\e` and the first
 * invalid character, inclusive.
 */

static int ansi_escape_width(uint32_t *s, int max_length)
{
    int i;
    
    if (*s != (CONTROL_TAG | ((uint32_t) '\e'))) {
       return 0;
    } else if (max_length <= 1) {
       return 1;
    } else if (s[1] != '[') {
       return 2;
    }
    
    for (i = 2; i < max_length && (s[i] == ';' || (s[i] >= '0' && s[i] <= '9')); i++);

    return i + 1;
}

static int cp_pos_to_col(TTY_ctx_t* ctx, int cp_pos)
{
    /*
     * If we don't have any character width information. Assume that
     * code points are one column wide
     */
    int w = 1;
    int col = 0;
    int i = 0;
    int j;

    if (cp_pos > ctx->llen) {
        col += cp_pos - ctx->llen;
        cp_pos = ctx->llen;
    }

    while (i < cp_pos) {
       j = ansi_escape_width(ctx->lbuf + i, ctx->llen - i);

       if (j > 0) {
           i += j;
       } else {
#ifdef HAVE_WCWIDTH
           w = wcwidth(lbuf[i]);
#endif
           if (w > 0) {
              col += w;
           }
           i++;
       }
    }

    return col;    
}

static int start_termcap(TTY_ctx_t* ctx)
{
    int eres;
    size_t envsz = 1024;
    char *env = NULL;
    char *c;
    int tres;

    DEBUGLOG(("start_termcap: .."));

    ctx->capbuf = driver_alloc(1024);
    if (!ctx->capbuf)
	goto termcap_false;
    eres = erl_drv_getenv("TERM", ctx->capbuf, &envsz);
    if (eres == 0)
	env = ctx->capbuf;
    else if (eres < 0) {
        DEBUGLOG(("start_termcap: failure in erl_drv_getenv(\"TERM\", ..) = %d\n", eres));
	goto termcap_false;
    } else /* if (eres > 1) */ {
      char *envbuf = driver_alloc(envsz);
      if (!envbuf)
	  goto termcap_false;
      while (1) {
	  char *newenvbuf;
	  eres = erl_drv_getenv("TERM", envbuf, &envsz);
	  if (eres == 0)
	      break;
	  newenvbuf = driver_realloc(envbuf, envsz);
          if (eres < 0 || !newenvbuf) {
              DEBUGLOG(("start_termcap: failure in erl_drv_getenv(\"TERM\", ..) = %d or realloc buf == %p\n", eres, newenvbuf));
	      env = newenvbuf ? newenvbuf : envbuf;
	      goto termcap_false;
	  }
	  envbuf = newenvbuf;
      }
      env = envbuf;
    }
    if ((tres = tgetent((char*)ctx->lbuf, env)) <= 0) {
        DEBUGLOG(("start_termcap: failure in tgetent(..) = %d\n", tres));
        goto termcap_false;
    }
    if (env != ctx->capbuf) {
	env = NULL;
	driver_free(env);
    }
    c = ctx->capbuf;
    ctx->cols = tgetnum("co");
    if (ctx->cols <= 0)
	ctx->cols = DEF_WIDTH;
    ctx->xn = tgetflag("xn");
    ctx->up = tgetstr("up", &c);
    if (!(ctx->down = tgetstr("do", &c)))
	ctx->down = "\n";
    if (!(ctx->left = tgetflag("bs") ? "\b" : tgetstr("bc", &c)))
	ctx->left = "\b";	/* Can't happen - but does on Solaris 2 */
    ctx->right = tgetstr("nd", &c);
    if (ctx->up && ctx->down && ctx->left && ctx->right) {
        DEBUGLOG(("start_termcap: successful start\n"));
        return 1;
    }
    DEBUGLOG(("start_termcap: failed start\n"));
termcap_false:
    if (env && env != ctx->capbuf)
	driver_free(env);
    if (ctx->capbuf)
	driver_free(ctx->capbuf);
    ctx->capbuf = NULL;
    return 0;
}

static int stop_termcap(TTY_ctx_t* ctx)
{
    if (ctx->capbuf) driver_free(ctx->capbuf);
    ctx->capbuf = NULL;
    return 1;
}

static int move_left(TTY_ctx_t* ctx, int n)
{
    while (n-- > 0)
	tputs(ctx->left, 1, outc);
    return 1;
}

static int move_right(TTY_ctx_t* ctx, int n)
{
    while (n-- > 0)
	tputs(ctx->right, 1, outc);
    return 1;
}

static int move_up(TTY_ctx_t* ctx, int n)
{
    while (n-- > 0)
	tputs(ctx->up, 1, outc);
    return 1;
}

static int move_down(TTY_ctx_t* ctx, int n)
{
    while (n-- > 0)
	tputs(ctx->down, 1, outc);
    return 1;
}


/*
 * Updates cols if terminal has resized (SIGWINCH). Should be called
 * at the start of any function that uses the COL or LINE macros. If
 * the terminal is resized after calling this function but before use
 * of the macros, then we may write to the wrong screen location.
 *
 * We cannot call this from the SIGWINCH handler because it uses
 * ioctl() which is not a safe function as listed in the signal(7)
 * man page.
 */
static void update_cols(TTY_ctx_t* ctx)
{
    uint32_t width, height;
 
    if (ctx->cols_needs_update) {
	ctx->cols_needs_update = 0;
	tty_get_window_size(ctx, &width, &height);
	ctx->cols = width;
    }
}


/*
 * Put a terminal device into non-canonical mode with ECHO off.
 * Before doing so we first save the terminal's current mode,
 * assuming the caller will call the tty_reset() function
 * (also in this file) when it's done with raw mode.
 */

static int tt_init(TTY_ctx_t* ctx, int canon, int echo, int sig) {
    int tres;
    DEBUGLOG(("tt_init: fd = %d, canon = %d, echo = %d, sig = %d", ctx->fd, canon, echo, sig));
    if ((tres = tcgetattr(ctx->fd, &ctx->rmode)) < 0) {
        DEBUGLOG(("tt_init: failure in tcgetattr(%d,..) = %d\n", ctx->fd, tres));
        return -1;
    }
    ctx->smode = ctx->rmode;

    /* Default characteristics for all usage including termcap output. */
    ctx->smode.c_iflag &= ~ISTRIP;

    /* Turn canonical (line mode) on off. */
    if (canon > 0) {
	ctx->smode.c_iflag |= ICRNL;
	ctx->smode.c_lflag |= ICANON;
	ctx->smode.c_oflag |= OPOST;
	ctx->smode.c_cc[VEOF] = ctx->rmode.c_cc[VEOF];
#ifdef VDSUSP
	ctx->smode.c_cc[VDSUSP] = ctx->rmode.c_cc[VDSUSP];
#endif
    }
    if (canon < 0) {
	ctx->smode.c_iflag &= ~ICRNL;
	ctx->smode.c_lflag &= ~ICANON;
	ctx->smode.c_oflag &= ~OPOST;
	/* Must get these really right or funny effects can occur. */
	ctx->smode.c_cc[VMIN] = 1;
	ctx->smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
	ctx->smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    if (echo > 0)
	ctx->smode.c_lflag |= ECHO;
    if (echo < 0)
	ctx->smode.c_lflag &= ~ECHO;

    /* Set extra characteristics for "RAW" mode, no signals. */
    if (sig > 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	ctx->smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	ctx->smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
	ctx->smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (sig < 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	ctx->smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	ctx->smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif	
	ctx->smode.c_lflag &= ~(ISIG|IEXTEN);
    }
    DEBUGLOG(("tt_init: successful init\n"));
    return 0;
}

/*
 * Set/restore a terminal's mode to whatever it was on the most
 * recent call to the tty_init() function above.
 */

static int tt_set(TTY_ctx_t* ctx)
{
    int tres;

    if ((tres = tcsetattr(ctx->fd, TCSANOW, &ctx->smode)) < 0) {
        DEBUGLOG(("tty_set: failure in tcgetattr(%d,..) = %d\n", ctx->fd, tres));
	return(-1);
    }
    return(0);
}

static int tt_reset(TTY_ctx_t* ctx)         /* of terminal device */
{
    int tres;

    if ((tres = tcsetattr(ctx->fd, TCSANOW, &ctx->rmode)) < 0) {
	return(-1);
    }
    return(0);
}

/* 
 * Signal handler to cope with signals so that we can reset the tty
 * to the original settings
 */

static void cont(int sig)
{
    // how do we know which tty to reset?
    if ((gctx != NULL) && (tt_set(gctx) < 0)) {
        DEBUGLOG(("signal: failure in cont(%d), can't set tty raw %d\n", sig, gctx->fd));
	fprintf(stderr,"Can't set tty raw\n");
	exit(1);
    }
}

static void winch(int sig)
{
    if (gctx)
	gctx->cols_needs_update = 1;
}

DRIVER_INIT(tty_drv)
{
    ErlDrvEntry* ptr = &tty_drv_entry;

    memset(ptr, 0, sizeof(ErlDrvEntry));
    ptr->init  = tty_init;
    ptr->start = tty_start;
    ptr->stop  = tty_stop;
    ptr->output = tty_from_erlang;
    ptr->ready_input  = tty_input_ready;
    ptr->ready_output = tty_output_ready;
    ptr->driver_name = "tty_drv";
    ptr->control = tty_control;
    ptr->flush = tty_flush_tty;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->stop_select = tty_stop_select;    
    return ptr;
}
