# splits


## TODOs / Limitations

* There is currently no way to provide different parsing options for each of
  the two CSV input files -- they are assumed to be in the same format.
* CSV parsing is *extremely* rudimentary. For example, there's currently no
  way to escape data that includes the CSV separator character in it (data
  with a comma in it, say). We're simply splitting the string on the
  separator. (Check out
  https://www.stackage.org/lts-19.25/package/parsec-3.1.14.0 ?)
* Instead of the user specifying a date format, we could try to guess it based
  on the shape of the data alone. Could also allow arbitrary date strings
  instead of the limited set of valid date formats currently allowed
  (https://www.stackage.org/lts-19.25/package/fuzzy-dates-0.1.1.2 or
  https://hackage.haskell.org/package/dates-0.2.3.2/docs/Data-Dates.html ?)
