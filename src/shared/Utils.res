type cookiePair = (string, option<string>)

let parseCookies = (): array<cookiePair> => {
  let cookie =
    Webapi.Dom.document
    ->Webapi.Dom.Document.asHtmlDocument
    ->Option.getOrThrow
    ->Webapi.Dom.HtmlDocument.cookie

  cookie
  ->String.split(";")
  ->Array.map(segment => {
    let pair = segment->String.split("=")
    let key = pair->Array.get(0)->Option.getOrThrow
    let value = pair->Array.get(1)
    (key, value)
  })
}

let getCookie = (name: string): option<cookiePair> =>
  parseCookies()->Array.find(pair => {
    let key = Pair.first(pair)
    key == name
  })

let setCookieRaw = (
  ~key: string,
  ~value: option<string>=?,
  ~expires: string,
  ~path: option<string>=?,
  (),
): unit => {
  let htmlDocument = Webapi.Dom.document->Webapi.Dom.Document.asHtmlDocument->Option.getOrThrow

  let value = value->Option.getOr("")
  let expires = expires !== "" ? `expires=${expires};` : ""
  let path =
    path
    ->Option.flatMap(path => path == "" ? None : Some(path))
    ->Option.map(path => ` path=${path};`)
    ->Option.getOr("")
  let cookie = `${key}=${value};${expires}${path}`

  Webapi.Dom.HtmlDocument.setCookie(htmlDocument, cookie)
}

let setCookie = (key: string, value: option<string>): unit => {
  open Constant

  let expires = Date.fromTime(Date.now() +. Duration.monthInMs)

  setCookieRaw(~key, ~value?, ~expires=expires->Date.toUTCString, ~path="/", ())
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

let formatDate = (date: Date.t): string => {
  let yyyy = date->Date.getFullYear->Int.toString
  let mm = date->Date.getMonth->Int.toString
  let dd = date->Date.getDate->Int.toString

  `${yyyy}/${mm}/${dd}`
}

module Json = {
  let decodeArrayString = (json: option<JSON.t>): option<array<string>> =>
    json
    ->Option.flatMap(JSON.Decode.array)
    ->Option.map(xs => xs->Array.filterMap(JSON.Decode.string))
}
