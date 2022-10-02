type location'

type onClickAction =
  | Location(location')
  | CustomFn(unit => unit)

let customFn: (unit => unit) => onClickAction
let location: location' => onClickAction

external toString: location' => string = "%identity"

let home: location'
let settings: location'
let register: location'
let login: location'
let createArticle: location'
let editArticle: (~slug: string) => location'
let article: (~slug: string) => location'
let profile: (~username: string) => location'
let favorited: (~username: string) => location'

let push: location' => unit
let availableIf: (bool, onClickAction) => onClickAction

@react.component
let make: (
  ~className: string=?,
  ~style: ReactDOM.Style.t=?,
  ~onClick: onClickAction,
  ~children: React.element,
) => React.element

module Button: {
  @react.component
  let make: (
    ~className: string=?,
    ~style: ReactDOM.Style.t=?,
    ~onClick: onClickAction,
    ~disabled: bool=?,
    ~children: React.element,
  ) => React.element
}
