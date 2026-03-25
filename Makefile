SBCL ?= sbcl

.PHONY: build clean

build: typecast

typecast: build.lisp typecast.asd src/core/*.lisp src/rust-ratatui/*.lisp
	$(SBCL) --noinform --non-interactive --load build.lisp

clean:
	rm -f typecast
	rm -rf test-output/
