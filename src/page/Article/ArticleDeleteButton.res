@react.component
let make = (~isBusy, ~onClick) =>
  <Link.Button
    className="btn btn-outline-danger btn-sm"
    onClick
    style={marginLeft: "5px"}
  >
    <i
      className={isBusy ? "ion-load-a" : "ion-trash-a"}
      style={marginRight: "5px"}
    />
    {"Delete Article"->React.string}
  </Link.Button>
