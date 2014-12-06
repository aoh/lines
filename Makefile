DESTDIR=
PREFIX=/usr
BINDIR=/bin
INSTALL=install
CFLAGS=-Wall -O2
OFLAGS=-O2


everything: lines .stamp

owl-lisp/bin/ol:
	# fetching the compiler. you can symlink owl-lisp
	# here if you already have it to avoid a lengthy 
	# first build
	-git clone https://github.com/aoh/owl-lisp.git
	-cd owl-lisp && git pull 
	cd owl-lisp && make

blab/bin/blab: owl-lisp/bin/ol
	# building blab (used for test material generation)
	-git clone https://github.com/aoh/blab.git
	-cd blab && ln -s ../owl-lisp && git pull 
	cd blab && make

.stamp: lines blab/bin/blab
	./test.sh ./lines
	touch .stamp

lines: lines.c
	cc ${CFLAGS} -o lines lines.c

lines.c: lines.scm owl-lisp/bin/ol
	owl-lisp/bin/ol ${OFLAGS} -o lines.c lines.scm

clean:
	-rm -rf lines lines.c tmp .stamp

test:
	./test.sh ./lines.scm

install: lines
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	$(INSTALL) -m 755 lines $(DESTDIR)$(PREFIX)/bin/lines

uninstall:
	-rm $(DESTDIR)$(PREFIX)/bin/lines

