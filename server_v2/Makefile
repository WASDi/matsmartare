TMPFILE := $(shell mktemp)

all:
	stack build

run:
	stack exec matsmartare

ghcid:
	ghcid -c 'stack ghci matsmartare'

ghcid-test:
	ghcid -c 'stack ghci test/Spec.hs'

dependencies:
	stack dot --external > $(TMPFILE)
	xdot $(TMPFILE)
	rm $(TMPFILE)
