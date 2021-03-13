type assertions

@module("chai") external chai: assertions = "assert"

@send external equal: (assertions, 'a, 'a) => unit = "equal"
