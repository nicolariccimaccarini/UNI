C_SOURCES := client-td-connreuse.c server-concurrent-td-connreuse.c utils.c rxb.c
C_TARGETS := client-td-connreuse server-concurrent-td-connreuse
TODAY     := $(shell date +%Y%m%d)
ARCHIVE   := soluzione_compito_20230110_v$(TODAY).zip

JAVA_SOURCE := ClientTDConnreuse.java
JAVA_TARGET := ClientTDConnreuse

CFLAGS  := -std=c17 -O2 -g3 -Wall -Werror -pedantic -fsanitize=address,undefined -Wformat -Werror=format-security -Werror=array-bounds
LDFLAGS := -fsanitize=address,undefined -fpie -Wl,-pie

.PHONY: clean all dist run-java

all: $(C_TARGETS) $(JAVA_TARGET)

# Regole per i target C
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

# Regole per il target Java
$(JAVA_TARGET): $(JAVA_SOURCE)
	javac $<

run-java: $(JAVA_TARGET)
	java $(JAVA_TARGET)

clean:
	@rm -rf $(C_TARGETS) *.zip *.o *.dSYM *~ $(JAVA_TARGET).class

dist:
	@zip $(ARCHIVE) $(C_SOURCES) $(JAVA_SOURCE) Makefile