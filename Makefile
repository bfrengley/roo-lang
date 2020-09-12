BINPATH=.

build: app/Roo.hs src/AST.hs src/Parser.hs src/PrettyPrint.hs
	stack build --copy-bins --local-bin-path $(BINPATH)

.PHONY: clean

clean:
	rm $(BINPATH)/Roo
