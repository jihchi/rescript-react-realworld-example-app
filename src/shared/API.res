open Promise
open Fetch

module Action = {
  type article =
    | Create(Shape.Article.t)
    | Read(string)
    | Update(string, Shape.Article.t)
    | Delete(string)

  type follow =
    | Follow(string)
    | Unfollow(string)

  type favorite =
    | Favorite(string)
    | Unfavorite(string)
}

module Helpers = {
  let addJwtToAuthorization: unit => array<(string, string)> = () =>
    Utils.getCookie("jwtToken")
    ->Option.flatMap(snd)
    ->Option.map(token => [("Authorization", `Token ${token}`)])
    ->Option.getWithDefault([])

  let addJsonToContentType: unit => array<(string, string)> = () => [
    ("Content-Type", "application/json; charset=UTF-8"),
  ]
}

let getErrorBodyJson: result<Js.Json.t, Response.t> => Promise.t<
  result<Js.Json.t, AppError.t>,
> = x =>
  switch x {
  | Ok(_json) as ok => ok->resolve
  | Error(resp) =>
    resp
    ->Response.json
    ->then(json => {
      let status = Response.status(resp)
      let statusText = Response.statusText(resp)
      let bodyJson = #json(json)

      AppError.fetch((status, statusText, bodyJson))->Result.Error->resolve
    })
  }

let getErrorBodyText: result<Js.Json.t, Response.t> => Promise.t<
  result<Js.Json.t, AppError.t>,
> = x =>
  switch x {
  | Ok(_json) as ok => ok->resolve
  | Error(resp) =>
    let status = Response.status(resp)
    let statusText = Response.statusText(resp)
    let bodyText = #text("FIXME: show body text instead")

    AppError.fetch((status, statusText, bodyText))->Result.Error->resolve
  }

let parseJsonIfOk: Response.t => Promise.t<result<Js.Json.t, Response.t>> = resp =>
  if Response.ok(resp) {
    resp
    ->Response.json
    ->then(json => json->Ok->resolve)
    ->catch(_error => resp->Result.Error->resolve)
  } else {
    resp->Result.Error->resolve
  }

let article: (~action: Action.article, unit) => Promise.t<result<Shape.Article.t, AppError.t>> = (
  ~action,
  (),
) => {
  let body = switch action {
  | Create(article) | Update(_, article) =>
    let article =
      list{
        ("title", Js.Json.string(article.title)),
        ("description", Js.Json.string(article.description)),
        ("body", Js.Json.string(article.body)),
        ("tagList", Js.Json.stringArray(article.tagList)),
      }
      ->Js.Dict.fromList
      ->Js.Json.object_

    list{("article", article)}->Js.Dict.fromList->Js.Json.object_->Js.Json.stringify->Body.string
  | Read(_) | Delete(_) => Body.none
  }

  let method__ = switch action {
  | Create(_) => #POST
  | Read(_) => #GET
  | Update(_) => #PUT
  | Delete(_) => #DELETE
  }

  let headers =
    switch action {
    | Create(_) | Update(_) => Helpers.addJsonToContentType()
    | Read(_) | Delete(_) => []
    }
    ->Array.concat(Helpers.addJwtToAuthorization())
    ->Headers.Init.array
    ->Headers.make

  let slug = switch action {
  | Create(_) => ""
  | Read(slug) | Update(slug, _) | Delete(slug) => slug
  }

  fetch(
    Endpoints.Articles.article(~slug, ()),
    {
      method: method__,
      headers,
      body,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyJson)
  ->then(result => {
    result
    ->Result.flatMap(json => {
      try {
        json
        ->Js.Json.decodeObject
        ->Option.getExn
        ->Js.Dict.get("article")
        ->Option.getExn
        ->Shape.Article.decode
        ->AppError.decode
      } catch {
      | _ => AppError.decode(Error("API.article: failed to decode json"))
      }
    })
    ->resolve
  })
}

let favoriteArticle: (
  ~action: Action.favorite,
  unit,
) => Promise.t<result<Shape.Article.t, AppError.t>> = (~action, ()) => {
  let endpoint = Endpoints.Articles.favorite(
    ~slug=switch action {
    | Favorite(slug) => slug
    | Unfavorite(slug) => slug
    },
    (),
  )

  fetch(
    endpoint,
    {
      method: switch action {
      | Favorite(_slug) => #POST
      | Unfavorite(_slug) => #DELETE
      },
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result
    ->Result.flatMap(json =>
      try {
        json
        ->Js.Json.decodeObject
        ->Option.getExn
        ->Js.Dict.get("article")
        ->Option.getExn
        ->Shape.Article.decode
        ->AppError.decode
      } catch {
      | _ => AppError.decode(Error("API.favoriteArticle: failed to decode json"))
      }
    )
    ->resolve
  )
}

let listArticles: (
  ~limit: int=?,
  ~offset: int=?,
  ~tag: string=?,
  ~author: string=?,
  ~favorited: string=?,
  unit,
) => Promise.t<result<Shape.Articles.t, AppError.t>> = (
  ~limit=10,
  ~offset=0,
  ~tag=?,
  ~author=?,
  ~favorited=?,
  (),
) => {
  let endpoint = Endpoints.Articles.root(~limit, ~offset, ~tag?, ~author?, ~favorited?, ())

  fetch(
    endpoint,
    {
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.Articles.decode->AppError.decode)->resolve
  )
}

let feedArticles: (
  ~limit: int=?,
  ~offset: int=?,
  unit,
) => Promise.t<result<Shape.Articles.t, AppError.t>> = (~limit=10, ~offset=0, ()) => {
  fetch(
    Endpoints.Articles.feed(~limit, ~offset, ()),
    {
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.Articles.decode->AppError.decode)->resolve
  )
}

let tags: unit => Promise.t<result<Shape.Tags.t, AppError.t>> = () =>
  fetch(
    Endpoints.tags,
    {
      method: #GET,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.Tags.decode->AppError.decode)->resolve
  )

let currentUser: unit => Promise.t<result<Shape.User.t, AppError.t>> = () => {
  fetch(
    Endpoints.user,
    {
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.User.decode->AppError.decode)->resolve
  )
}

let updateUser: (
  ~user: Shape.User.t,
  ~password: string,
  unit,
) => Promise.t<result<Shape.User.t, AppError.t>> = (~user, ~password, ()) => {
  let user =
    list{
      list{("email", Js.Json.string(user.email))},
      list{("bio", Js.Json.string(user.bio->Option.getWithDefault("")))},
      list{("image", Js.Json.string(user.image->Option.getWithDefault("")))},
      list{("username", Js.Json.string(user.username))},
      if password == "" {
        list{}
      } else {
        list{("password", Js.Json.string(password))}
      },
    }
    ->List.flatten
    ->Js.Dict.fromList
    ->Js.Json.object_

  let body = list{("user", user)}->Js.Dict.fromList->Js.Json.object_->Js.Json.stringify->Body.string

  fetch(
    Endpoints.user,
    {
      method: #PUT,
      headers: Helpers.addJwtToAuthorization()
      ->Array.concat(Helpers.addJsonToContentType())
      ->Headers.Init.array
      ->Headers.make,
      body,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyJson)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.User.decode->AppError.decode)->resolve
  )
}

let followUser: (~action: Action.follow, unit) => Promise.t<result<Shape.Author.t, AppError.t>> = (
  ~action,
  (),
) => {
  fetch(
    Endpoints.Profiles.follow(
      ~username=switch action {
      | Follow(username) | Unfollow(username) => username
      },
      (),
    ),
    {
      method: switch action {
      | Follow(_username) => #POST
      | Unfollow(_username) => #DELETE
      },
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result
    ->Result.flatMap(json => {
      try {
        json
        ->Js.Json.decodeObject
        ->Option.getExn
        ->Js.Dict.get("profile")
        ->Option.getExn
        ->Shape.Author.decode
        ->AppError.decode
      } catch {
      | _ => AppError.decode(Result.Error("API.followUser: failed to decode json"))
      }
    })
    ->resolve
  )
}

let getComments: (~slug: string, unit) => Promise.t<result<array<Shape.Comment.t>, AppError.t>> = (
  ~slug,
  (),
) => {
  fetch(
    Endpoints.Articles.comments(~slug, ()),
    {
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.Comment.decode->AppError.decode)->resolve
  )
}

let deleteComment: (
  ~slug: string,
  ~id: int,
  unit,
) => Promise.t<result<(string, int), AppError.t>> = (~slug, ~id, ()) => {
  fetch(
    Endpoints.Articles.comment(~slug, ~id, ()),
    {
      method: #DELETE,
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result => result->Result.flatMap(_json => Result.Ok((slug, id)))->resolve)
}

let addComment: (
  ~slug: string,
  ~body: string,
  unit,
) => Promise.t<result<Shape.Comment.t, AppError.t>> = (~slug, ~body, ()) => {
  let comment = list{("body", Js.Json.string(body))}->Js.Dict.fromList->Js.Json.object_

  let body =
    list{("comment", comment)}->Js.Dict.fromList->Js.Json.object_->Js.Json.stringify->Body.string

  fetch(
    Endpoints.Articles.comments(~slug, ()),
    {
      method: #POST,
      headers: Helpers.addJwtToAuthorization()
      ->Array.concat(Helpers.addJsonToContentType())
      ->Headers.Init.array
      ->Headers.make,
      body,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result
    ->Result.flatMap(json => {
      try {
        json
        ->Js.Json.decodeObject
        ->Option.getExn
        ->Js.Dict.get("comment")
        ->Option.getExn
        ->Shape.Comment.decodeComment
        ->AppError.decode
      } catch {
      | _ => AppError.decode(Result.Error("API.addComment: failed to decode json"))
      }
    })
    ->resolve
  )
}

let getProfile: (~username: string, unit) => Promise.t<result<Shape.Author.t, AppError.t>> = (
  ~username,
  (),
) => {
  fetch(
    Endpoints.Profiles.profile(~username, ()),
    {
      headers: Helpers.addJwtToAuthorization()->Headers.Init.array->Headers.make,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyText)
  ->then(result =>
    result
    ->Result.flatMap(json => {
      try {
        json
        ->Js.Json.decodeObject
        ->Option.getExn
        ->Js.Dict.get("profile")
        ->Option.getExn
        ->Shape.Author.decode
        ->AppError.decode
      } catch {
      | _ => AppError.decode(Result.Error("API.getProfile: failed to decode json"))
      }
    })
    ->resolve
  )
}

let login = (~email: string, ~password: string, ()): Promise.t<
  result<Shape.User.t, AppError.t>,
> => {
  let user =
    list{("email", Js.Json.string(email)), ("password", Js.Json.string(password))}
    ->Js.Dict.fromList
    ->Js.Json.object_

  let body = list{("user", user)}->Js.Dict.fromList->Js.Json.object_->Js.Json.stringify->Body.string

  fetch(
    Endpoints.Users.login,
    {
      method: #POST,
      headers: Helpers.addJsonToContentType()->Headers.Init.array->Headers.make,
      body,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyJson)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.User.decode->AppError.decode)->resolve
  )
}

let register: (
  ~username: string,
  ~email: string,
  ~password: string,
  unit,
) => Promise.t<result<Shape.User.t, AppError.t>> = (~username, ~email, ~password, ()) => {
  let user =
    list{
      ("email", Js.Json.string(email)),
      ("password", Js.Json.string(password)),
      ("username", Js.Json.string(username)),
    }
    ->Js.Dict.fromList
    ->Js.Json.object_

  let body = list{("user", user)}->Js.Dict.fromList->Js.Json.object_->Js.Json.stringify->Body.string

  fetch(
    Endpoints.Users.root,
    {
      method: #POST,
      headers: Helpers.addJsonToContentType()->Headers.Init.array->Headers.make,
      body,
    },
  )
  ->then(parseJsonIfOk)
  ->then(getErrorBodyJson)
  ->then(result =>
    result->Result.flatMap(json => json->Shape.User.decode->AppError.decode)->resolve
  )
}
