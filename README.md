

![Build](https://github.com/lindig/fit/workflows/CI/badge.svg)

# FIT

This is a minimal [OCaml] project to parse FIT files as they are
produced by personal fitness devices. FIT is a binary format that groups
basic values in records, which typically include a timestamp.

    {
      "msg": "20",
      "13": 11,
      "2": 1900,
      "5": 1732414,
      "6": 0,
      "1": 1669620,
      "0": 622905943,
      "253": 971857351
    }

Each record has a global message number (20) which defines the purpose
of the record and a number of values in position slots. The meaning of
these is defined in the FIT Protocol but this library (so far) only
implements the parsing. For example, 20 message is called _record_ in
the FIT protocol and slots have these meanings:

* 0 position\_lat
* 1 position\_long
* 2 altitude
* 5 distance
* 6 speed
* 13 temperature
* 253 timestamp

Values are further scaled and shifted, which is also defined in the
protocol, and this transformation is not (yet) implemented in the
library.

# Resources

* https://developer.garmin.com/fit/protocol/
* https://www.pinns.co.uk/osm/fit-for-dummies.html
# Contribute

If you find this useful, please contribute back by raising pull
requests for improvements you made.

[OCaml]:  https://www.ocaml.org/
