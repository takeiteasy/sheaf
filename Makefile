SBCL ?= sbcl
QUICKLISP ?= $(HOME)/quicklisp/setup.lisp

# The source registry puts vendor/coalton ahead of any Coalton Quicklisp has.
LISP = $(SBCL) --non-interactive --load $(QUICKLISP) \
	--eval '(asdf:initialize-source-registry (quote (:source-registry (:tree "$(CURDIR)/") :inherit-configuration)))'

.PHONY: test
test:
	$(LISP) --eval '(ql:quickload "sheaf/tests" :silent t)' \
		--eval '(uiop:quit (if (uiop:symbol-call (quote #:sheaf/tests) (quote #:run)) 0 1))'
