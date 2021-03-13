open ReactTestingLibrary
open Mocha
open Chai

it("renders without crashing", () => {
  render(<App />)

  waitFor(() => {
    screen->getByText("A place to share your knowledge.")->ignore
  })->Promise.then(() => {
    chai->equal(
      screen->getByText("A place to share your knowledge.")->getNodeText,
      "A place to share your knowledge.",
    )
    Promise.resolve()
  })
})
