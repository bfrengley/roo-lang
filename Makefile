# This file is used to build the project on the university system
# To build it locally for development, use `stack build`
#
# Stewart Webb - sjwebb@student.unimelb.edu.au
# Ben Frengley - bfrengley@student.unimelb.edu.au

BINPATH=.
STACK_YAML=stack.submit.yaml
PROXY=proxy.unimelb.edu.au:8000

build: app/Roo.hs src/AST.hs src/Parser.hs src/PrettyPrint.hs
	HTTPS_PROXY=$(PROXY)
	stack build --stack-yaml $(STACK_YAML) --copy-bins

.PHONY: clean purge

clean:
	stack clean && rm $(BINPATH)/Roo

purge: clean
	stack purge
