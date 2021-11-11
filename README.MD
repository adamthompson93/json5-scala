Without using a JSON5 library or similarly cheating, implement a parser for a superset of JSON with the following additional allowances:
* Map field keys that consist solely of alphanumeric or underscore characters do not require quotes.
* A null list element may be denoted by the empty expression.  However, the token sequence (“[“,“]”) should be interpreted as the empty list, and specifically it should not be interpreted as equivalent to the JSON "[null]”.
* Carriage returns after array values or map field values act like commas unless the last token on the line was a comma.
* Single-line comments begin with “//“ and continue to the end of the line.  The contents of single-line comments are disregarded.
* Multi-line comments begin with “/*” and continue to the matching “*/“.  Multi-line comments may be nested.  The contents of multi-line comments are disregarded.