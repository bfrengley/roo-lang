# This file is used to build the project on the university system
#
# Stewart Webb - sjwebb@student.unimelb.edu.au
# Ben Frengley - bfrengley@student.unimelb.edu.au

PROXY=proxy.unimelb.edu.au:8000

build: Roo.hs Parser.hs AST.hs PrettyPrint.hs
	http_proxy=$(PROXY) https_proxy=$(PROXY) cabal build  && \
	cp dist/build/Roo/Roo .

Roo: build
	# empty rule

.PHONY: clean

clean:
	rm $(BINPATH)/Roo
