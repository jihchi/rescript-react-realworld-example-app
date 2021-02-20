type assertions
type result

@module("@testing-library/react") external render: React.element => unit = "render"

@module("@testing-library/react") external getNodeText: Dom.element => string = "getNodeText"

@module("@testing-library/react") @val external screen: result = "screen"

@module("@testing-library/react") @val
external waitFor: (unit => unit) => Js.Promise.t<unit> = "waitFor"

@send external getByText: (result, string) => Dom.element = "getByText"

@module("chai") external chaiAssert: assertions = "assert"

@send external equal: (assertions, 'a, 'a) => unit = "equal"

@val external it: (string, @uncurry (unit => Js.Promise.t<unit>)) => unit = "it"

it("renders without crashing", () => {
  render(<App />)
  waitFor(() => {
    let _ = screen->getByText("A place to share your knowledge.")
  })->Promise.then(() => {
    let actual = screen->getByText("A place to share your knowledge.")->getNodeText
    let expected = "A place to share your knowledge."
    chaiAssert->equal(actual, expected)
    Promise.resolve()
  })
})
