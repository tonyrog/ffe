#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=here
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = here_nif.o
LIBS = 

all: ../priv/$(APP)_nif.so

../priv/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o ../priv/$(APP)_nif.so

clean:
	rm -f $(OBJS) ../priv/$(APP)_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
