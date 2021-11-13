

![Build](https://github.com/lindig/fit/workflows/CI/badge.svg)

# FIT

This is a minimal [OCaml] project to parse FIT files as they are
produced by personal fitness devices. FIT is a binary format invented by
Garmin that groups basic values in records, which typically include a
timestamp.

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

Each record has a global message number (like 20) which defines the purpose
of the record and a number of values in position slots. The meaning of
these is defined in the FIT Protocol but this library (so far) only
implements the parsing. For example, message 20 is called _record_ in
the FIT protocol and slots have these meanings:

* 0 position\_lat
* 1 position\_long
* 2 altitude
* 5 distance
* 6 speed
* 13 temperature
* 253 timestamp

Values are further scaled and shifted, which is also defined in the
protocol, and this transformation is only implemented for a few fields
of the "record" message:

    {
      "msg": "record",
      "timestamp": "2020-10-17T06:25:19",
      "0": 622927716,
      "1": 1703145,
      "speed": 2.745,
      "distance": 277.23,
      "altitude": -118.8,
      "temperature": inf
    }

# CLI

This code is primarily intended as a library but it also provides a
small binary. The `fit` command emits the data to stdout in JSON format.
I am using this currently for inspecting FIT files. The FIT file in
`data/` is from a bike computer.

    $ fit data/xpress-4x-2020-10-17.fit | head -25
    [
      {
        "msg": "0",
        "3": 5122,
        "4": 971850094,
        "1": 267,
        "2": 1803,
        "5": 0,
        "0": 4
      },
      {
        "msg": "68",
        "0": 17,
        "1": 1,
        "2": 0,
        "3": 10,
        "4": 255,
        "5": 0,
        "6": 0,
        "7": 0,
        "8": 232,
        "9": 0,
        "10": 60,
        "11": 19693
      },
      ...

# Installing FIT

Version 1.0.1 has been published as an Opam package such that it can be
installed from Opam:

    opam install fit

You can also pin it directly for access to unpublished changes:

    opam pin add -y git+https://github.com/lindig/fit

Once installed, you can use it:

    $ utop -require fit -require rresult
    utop # open Rresult;;
    utop # Fit.read "data/xpress-4x-2020-10-17.fit" >>= fun fit ->
           Fit.to_json |> R.return;;

The `fit` binary takes a FIT file as argument:

    $ fit data/xpress-4x-2020-10-17.fit | tail -15
        "58": 11,
        "20": null,
        "21": null
      },
      {
        "msg": "activity",
        "timestamp": "2020-10-17T08:22:35",
        "0": 7263000,
        "5": 971860957,
        "1": 1,
        "2": 0,
        "3": 26,
        "4": 1
      }
    $ fit --help


# Resources

* https://developer.garmin.com/fit/protocol/
* https://www.pinns.co.uk/osm/fit-for-dummies.html
# Contribute

If you find this useful, please contribute back by raising pull
requests for improvements you made.

[OCaml]:  https://www.ocaml.org/
[Opam]:   https://opam.ocaml.org/
