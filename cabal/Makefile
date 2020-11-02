# This file is used to build the project on the university system
#
# Stewart Webb - sjwebb@student.unimelb.edu.au
# Ben Frengley - bfrengley@student.unimelb.edu.au

PROXY=proxy.unimelb.edu.au:8000
BINPATH=.

build: *.hs
	http_proxy=$(PROXY) https_proxy=$(PROXY) cabal build && \
	cp "$$(find . -type f -path './dist*' -name 'Roo')" $(BINPATH)

Roo: build
	# empty rule

.PHONY: clean

clean:
	rm -rf ./dist* && rm $(BINPATH)/Roo

