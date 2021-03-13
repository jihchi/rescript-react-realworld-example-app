type result

@module("@testing-library/react") external render: React.element => unit = "render"

@module("@testing-library/react") external getNodeText: Dom.element => string = "getNodeText"

@module("@testing-library/react") @val external screen: result = "screen"

@module("@testing-library/react") @val
external waitFor: (unit => unit) => Js.Promise.t<unit> = "waitFor"

@send external getByText: (result, string) => Dom.element = "getByText"
