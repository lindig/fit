# 1.2.0

* Replace Ezjsonm with Yojson, which is more widely used
* Replace ISO8601 with Ptime

# 1.1.0

* Remove Int32 of int32 representation for integer values, always use
  Int of int.
* Fix parsing of unsinged numerical values
* Provide parsing of strings (not just files)

# 1.0.4

* Emit NaN float values as JSON Null. Previously emitted as nan, which
  is not valid JSON.
* Decode "enhanced_speed" field in "record" messages for JSON output.
* Decode "cycle_length" and "total_cycles" in "record" messages.
* Add a simple jq(1) script that transforms JSON to CSV for some fields.
* Improve error handling for float values: NaN and infities are reported
  as Unknown values (and Null in JSON).
* Simplify code for parsing and decoding binary records.

# 1.0.3

* Improve README
* Use monadic let syntax in code
* Adapt for Cmdliner 1.1.x

# 1.0.1

* Add version constraints on dependencies

# 1.0.0

First public release
