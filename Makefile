.PHONY: all build pedantic test clean purge example

all: build

build:
	stack build

# All the warnings, all the errors. Good to run every so often to catch bugs.
pedantic:
	stack build --pedantic

test:
	stack test

# See https://docs.haskellstack.org/en/stable/GUIDE/#the-stack-clean-command
# for the differences between `clean` and `purge`.
clean:
	stack clean

purge:
	stack purge

# A sample run with included test data.
example: build
	stack exec splits -- -d 0 -a 5 -r 1 test/data/user1-test.csv test/data/user2-test.csv
