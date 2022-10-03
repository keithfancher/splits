# splits


## TODOs / Limitations

* Need a way to specify the date format, *guess* the date format, or both
  (https://www.stackage.org/lts-19.25/package/fuzzy-dates-0.1.1.2 or
  https://hackage.haskell.org/package/dates-0.2.3.2/docs/Data-Dates.html ?)
* There is currently no way to provide different parsing options for each of
  the two CSV input files -- they are assumed to be in the same format.
* CSV parsing is *extremely* rudimentary. For example, there's currently no
  way to escape data that includes the CSV separator character in it (data
  with a comma in it, say). We're simply splitting the string on the
  separator. (Check out
  https://www.stackage.org/lts-19.25/package/parsec-3.1.14.0 ?)
