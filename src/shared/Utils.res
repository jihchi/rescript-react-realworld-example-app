type cookiePair = (string, option<string>)

let secondInMs = 1000.
let minuteInMs = 60. *. secondInMs
let hourInMs = 60. *. minuteInMs
let dayInMs = 24. *. hourInMs
let monthInMs = 30. *. dayInMs

let parseCookies: unit => array<cookiePair> = () =>
  Webapi.Dom.document
  ->Webapi.Dom.Document.asHtmlDocument
  ->Option.getExn
  ->Webapi.Dom.HtmlDocument.cookie
  ->Js.String2.split(";")
  ->Js.Array2.reduce((acc, str) => {
    let pair = str->Js.String2.split("=")->Js.Array2.map(Js.String.trim)
    let key = pair->Array.getUnsafe(0)
    let value = pair->Array.get(1)

    acc->Js.Array2.concat([(key, value)])
  }, [])

let getCookie = (name: string): option<cookiePair> =>
  parseCookies()->Js.Array2.find(pair => {
    let key = fst(pair)
    key == name
  })

let setCookieRaw: (
  ~key: string,
  ~value: string=?,
  ~expires: string,
  ~path: string=?,
  unit,
) => unit = (~key, ~value=?, ~expires, ~path=?, ()) => {
  let htmlDocument = Webapi.Dom.document->Webapi.Dom.Document.asHtmlDocument->Option.getExn

  let value = value->Option.getWithDefault("")
  let expires = expires !== "" ? `expires=${expires};` : ""
  let path =
    path
    ->Option.flatMap(path => path == "" ? None : Some(path))
    ->Option.map(path => ` path=${path};`)
    ->Option.getWithDefault("")
  let cookie = `${key}=${value};${expires}${path}`

  Webapi.Dom.HtmlDocument.setCookie(htmlDocument, cookie)
}

let setCookie: (string, option<string>) => unit = (key, value) => {
  let expires = Js.Date.make()
  let _ = Js.Date.setTime(expires, Js.Date.getTime(expires) +. monthInMs)

  setCookieRaw(~key, ~value?, ~expires=expires->Js.Date.toUTCString, ~path="/", ())
}

let deleteCookie: string => unit = key =>
  setCookieRaw(~key, ~expires="Thu, 01 Jan 1970 00:00:01 GMT", ())

let isMouseRightClick = event =>
  !ReactEvent.Mouse.defaultPrevented(event) &&
  ReactEvent.Mouse.button(event) == 0 &&
  !ReactEvent.Mouse.altKey(event) &&
  !ReactEvent.Mouse.ctrlKey(event) &&
  !ReactEvent.Mouse.metaKey(event) &&
  !ReactEvent.Mouse.shiftKey(event)

let formatDate: Js.Date.t => string = date => {
  let yyyy = date->Js.Date.getFullYear->Int.fromFloat->Int.toString
  let mm = date->Js.Date.getMonth->Int.fromFloat->Int.toString
  let dd = date->Js.Date.getDate->Int.fromFloat->Int.toString

  `${yyyy}/${mm}/${dd}`
}

module Json = {
  let decodeArrayString = (json: option<Js.Json.t>): option<array<string>> =>
    json
    ->Option.flatMap(Js.Json.decodeArray)
    ->Option.map(xs => xs->Array.filterMap(Js.Json.decodeString))
}
