# Splits: An expense-splitting tool for no one

A simple tool, built primarily as an excuse to learn Haskell.

## What's it do?

In short, it totals up expenses for two people, then figures out who owes whom
on a monthly basis.

Here's the use-case: I share expenses with someone. Let's say, food expenses.
Lots of little transactions throughout the month. Every so often, maybe every
handful of months, we need to settle up. Each of us logs into our credit card
or bank site, filters the transactions (by category and date range), and
exports the data as CSVs.

I then feed both sets of data into `splits` and out comes a handy summary of
how much each of us spent per month as well as who owes whom. Then we pay each
other and live happily ever after.

## Why did you make this?

Well, I actually needed it. But more importantly, as I said above, this is
mostly a vehicle for me to learn Haskell. (In truth, the core functionality of
this tool could probably be replaced with some Excel formulas or something.)

## Build requirements

I think
[`stack`](https://docs.haskellstack.org/en/stable/#how-to-install-stack) is
the only hard requirement. (Which can alternatively be installed with
[GHCup](https://www.haskell.org/ghcup/) -- whatever floats your boat.)

Once you've got `stack` installed, it'll take care of the rest -- GHC versions
and project dependencies and so on.

## How to build

Simple: `stack build`! Or even more simply: `make`. Check out the `Makefile`
for some other useful stuff (cleaning, running, testing, etc.).

You can use `stack install` to install on your system -- it simply copies the
binary to the default Stack binary-place. (You may still need to add that to
your `PATH`. It's `~/.local/bin` on my machine.)

## Running it

From within the project directory, you can use `stack exec`. For example:

```
$ stack exec splits -- --help # the extra dashes are required when running via stack
```

Try `make example` to do a sample run with some test data. It should give you
the general idea, especially re: the available CLI options. (`splits` needs a
few basic bits of info about the shape of your CSV data.)

If you've installed via `stack install` (and the stack install directory is in
your path), you can just use it like any other binary:

```
$ splits -h
$ splits -d 0 -a 5 -r 1 mydata.csv yourdata.csv
```

## TODOs / Limitations

* Could use an option to output in CSV format as well -- probably more
  convenient for record keeping.
* There is currently no way to provide different parsing options for each of
  the two CSV input files -- they are assumed to be in the same format.
* Instead of the user specifying a date format, we could try to guess it based
  on the shape of the data alone. Could also allow arbitrary date strings
  instead of the limited set of valid date formats currently allowed. (Maybe
  check out
  [fuzzy-dates](https://www.stackage.org/lts-19.25/package/fuzzy-dates-0.1.1.2)
  or
  [Data.Dates](https://hackage.haskell.org/package/dates-0.2.3.2/docs/Data-Dates.html)?)
* You probably shouldn't use this :')
