#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = 
LIBS = 

all: ../priv/here_nif.so ../priv/tty_drv.so

../priv/here_nif.so: here_nif.o
	$(CC) $(LDFLAGS) here_nif.o $(LIBS) -o ../priv/here_nif.so

../priv/tty_drv.so: tty_drv.o
	$(CC) $(LDFLAGS) tty_drv.o -o ../priv/tty_drv.so

clean:
	rm -f $(OBJS) ../priv/here_nif.so ../priv/tty_drv.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
