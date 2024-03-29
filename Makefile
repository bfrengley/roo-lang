build-oz:
	$(MAKE) -C oz/ oz

build:
	stack build

EXAMPLE_FILE=examples/$(EXAMPLE).roo
compile-example:
	@if [ ! -f $(EXAMPLE_FILE) ]; then echo "Example $(EXAMPLE) does not exist" && exit 1; fi;
	@echo "Compiling $(EXAMPLE_FILE):"
	@cat $(EXAMPLE_FILE)
	stack run -- $(EXAMPLE_FILE)

run-example: build-oz
	@if [ ! -f $(EXAMPLE_FILE) ]; then echo "Example $(EXAMPLE) does not exist" && exit 1; fi;
	@echo "Compiling and running $(EXAMPLE_FILE):"
	@cat $(EXAMPLE_FILE) && echo
	stack run -- $(EXAMPLE_FILE) | tee examples/$(EXAMPLE)-compiled.oz
	oz/oz examples/$(EXAMPLE)-compiled.oz

clean:
	stack clean
	rm examples/*-compiled.oz