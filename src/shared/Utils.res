type cookiePair = (string, option<string>)

let parseCookies = (): array<cookiePair> => {
  let cookie =
    Webapi.Dom.document
    ->Webapi.Dom.Document.asHtmlDocument
    ->Option.getExn
    ->Webapi.Dom.HtmlDocument.cookie

  cookie
  ->String.split(";")
  ->Array.map(segment => {
    let pair = segment->String.split("=")
    let key = pair->Array.get(0)->Option.getExn
    let value = pair->Array.get(1)
    (key, value)
  })
}

let getCookie = (name: string): option<cookiePair> =>
  parseCookies()->Array.find(pair => {
    let key = fst(pair)
    key == name
  })

let setCookieRaw = (
  ~key: string,
  ~value: option<string>=?,
  ~expires: string,
  ~path: option<string>=?,
  (),
): unit => {
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

let setCookie = (key: string, value: option<string>): unit => {
  open Constant

  let expires = Js.Date.make()
  let _ = Js.Date.setTime(expires, Js.Date.getTime(expires) +. Duration.monthInMs)

  setCookieRaw(~key, ~value?, ~expires=expires->Js.Date.toUTCString, ~path="/", ())
}

let deleteCookie = (key: string): unit =>
  setCookieRaw(~key, ~expires="Thu, 01 Jan 1970 00:00:01 GMT", ())

let isMouseRightClick = event => {
  open ReactEvent

  !Mouse.defaultPrevented(event) &&
  Mouse.button(event) == 0 &&
  !Mouse.altKey(event) &&
  !Mouse.ctrlKey(event) &&
  !Mouse.metaKey(event) &&
  !Mouse.shiftKey(event)
}

let formatDate = (date: Js.Date.t): string => {
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
