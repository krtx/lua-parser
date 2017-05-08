.PHONY: all

all: main.byte

SOURCE = lexer.mll main.ml parser.mly syntax.ml

main.byte: $(SOURCE)
	ocamlbuild -use-menhir -menhir "menhir --dump --explain" main.byte

clean:
	ocamlbuild -clean
