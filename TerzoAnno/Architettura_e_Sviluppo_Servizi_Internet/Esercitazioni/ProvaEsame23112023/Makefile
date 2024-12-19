JAVA_SOURCES := $(wildcard *.java)
JAVA_TARGETS := $(patsubst %.java,%.class,$(JAVA_SOURCES))
C_SOURCES    := $(wildcard *.[ch])
C_TARGETS    := verifica_compilazione server-concurrent-td-connreuse 
TODAY        := $(shell date +%Y%m%d)
ARCHIVE      := soluzione_compito_20230110_v$(TODAY).zip

CFLAGS  := -std=c17 -O2 -g3 -Wall -Werror -pedantic -fsanitize=address,undefined -Wformat -Werror=format-security -Werror=array-bounds
LDFLAGS := -fsanitize=address,undefined -fpie -Wl,-pie

ifeq ($(USE_LIBUNISTRING),yes)
  CFLAGS += -DUSE_LIBUNISTRING=1
  LDFLAGS += -lunistring
endif

.PHONY: clean

all: $(C_TARGETS) $(JAVA_TARGETS)

# dependencies

verifica_compilazione: verifica_compilazione.o rxb.o utils.o
verifica_compilazione.o: verifica_compilazione.c rxb.h utils.h
server-concurrent-td-connreuse: server-concurrent-td-connreuse.o rxb.o utils.o
server-concurrent-td-connreuse.o: server-concurrent-td-connreuse.c rxb.h utils.h

%.class: %.java
	javac $<

clean:
	@rm -rf $(C_TARGETS) $(JAVA_TARGETS) *.zip *.o *.dSYM *~

dist:
	@zip $(ARCHIVE) $(C_SOURCES) $(JAVA_SOURCES) Compito* Makefile
