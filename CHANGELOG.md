
# 1.0.4

* Emit NaN float values as JSON Null. Previously emitted as nan, which
  is not valid JSON.
* Decode "enhanced_speed" field in "record" messages for JSON output.
* Decode "cycle_length" and "total_cycles" in "record" messages.
* Add a simple jq(1) script that transforms JSON to CSV for some fields.

# 1.0.3

* Improve README
* Use monadic let syntax in code
* Adapt for Cmdliner 1.1.x

# 1.0.1

* Add version constraints on dependencies

# 1.0.0

First public release
