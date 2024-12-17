C_SOURCES := client-td-connreuse.c server-concurrent-td-connreuse.c utils.c rxb.c
C_TARGETS := client-td-connreuse server-concurrent-td-connreuse
TODAY     := $(shell date +%Y%m%d)
ARCHIVE   := soluzione_compito_20230110_v$(TODAY).zip

CFLAGS  := -std=c17 -O2 -g3 -Wall -Werror -pedantic -fsanitize=address,undefined -Wformat -Werror=format-security -Werror=array-bounds
LDFLAGS := -fsanitize=address,undefined -fpie -Wl,-pie

.PHONY: clean all dist

all: $(C_TARGETS)

client-td-connreuse: client-td-connreuse.o rxb.o utils.o
	$(CC) $(LDFLAGS) -o $@ $^

client-td-connreuse.o: client-td-connreuse.c rxb.h utils.h
	$(CC) $(CFLAGS) -c $<

server-concurrent-td-connreuse: server-concurrent-td-connreuse.o rxb.o utils.o
	$(CC) $(LDFLAGS) -o $@ $^

server-concurrent-td-connreuse.o: server-concurrent-td-connreuse.c rxb.h utils.h
	$(CC) $(CFLAGS) -c $<

utils.o: utils.c utils.h
	$(CC) $(CFLAGS) -c $<

rxb.o: rxb.c rxb.h
	$(CC) $(CFLAGS) -c $<

clean:
	@rm -rf $(C_TARGETS) *.zip *.o *.dSYM *~

dist:
	@zip $(ARCHIVE) $(C_SOURCES) Makefile