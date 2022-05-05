# Project VUT-FIT FLP Functional project (simplify-bkg)
# Author Adam Švenk (xsvenk00)
# Date: 2022-04-02

CC=ghc
CFLAGS=--make -Wall -o $(NAME)
NAME=flp21-fun
LOGIN=xsvenk00
SRCPATH=src/

$(NAME):
	$(CC) $(CFLAGS) $(SRCPATH)Main.hs
	rm -f *.hi *.o

.PHONY: build
build: clean-bin $(NAME) clean

.PHONY: run
run:
	./$(NAME)

.PHONY: clean
clean:
	@rm -f $(SRCPATH)*.hi $(SRCPATH)*.o

.PHONY: clean-bin
clean-bin:
	@rm -f $(NAME)

.PHONY: clean-all
clean-all: clean-bin clean