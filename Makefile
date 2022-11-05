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

# A sample run with included test data. Note that the date formatting options
# (`-f` and `-s`) wouldn't actually *have* to be included with this run, since
# our data matches the default date-parsing settings. But including anyway, to
# show how it would look.
#
# -d 0:   The expense date is found at index `0` of the row
# -a 5:   The expense amount is found at index `5` of the row
# -r 1:   The actual data starts at row `1` -- in other words, row `0` is the header
# -f MDY: The order of date fields is "month day year"
# -s "/": The character that separates the date fields is "/"
example: build
	stack exec splits -- -d 0 -a 5 -r 1 -f MDY -s "/" test/data/user1-test.csv test/data/user2-test.csv
