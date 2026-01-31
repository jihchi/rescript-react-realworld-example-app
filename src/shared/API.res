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

type parsedBody = result<JSON.t, Response.t>
type extractedError = result<JSON.t, AppError.t>

let addJwtAuthorization = (): array<(string, string)> => {
  Utils.getCookie(Constant.Auth.tokenCookieName)
  ->Option.flatMap(Pair.second)
  ->Option.map(token => [("Authorization", `Token ${token}`)])
  ->Option.getOr([])
}

let addJsonContentType = (): array<(string, string)> => {
  [("Content-Type", "application/json; charset=UTF-8")]
}

let parseBodyAsJson = (response: Response.t): Promise.t<parsedBody> => {
  if Response.ok(response) {
    response
    ->Response.json
    ->then(json => json->Ok->resolve)
    ->catch(_error => response->Result.Error->resolve)
  } else {
    response->Result.Error->resolve
  }
}

let extractJsonError = (result: parsedBody): Promise.t<extractedError> => {
  switch result {
  | Ok(json) => json->Result.Ok->resolve
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
}

let extractTextError = (result: parsedBody): Promise.t<extractedError> => {
  switch result {
  | Ok(json) => json->Result.Ok->resolve
  | Error(resp) =>
    resp
    ->Response.text
    ->then(text => {
      let status = Response.status(resp)
      let statusText = Response.statusText(resp)
      let bodyText = #text(text)

      AppError.fetch((status, statusText, bodyText))->Result.Error->resolve
    })
  }
}

let article = async (~action: Action.article, ()): result<Shape.Article.t, AppError.t> => {
  let url = Endpoints.Articles.article(
    ~slug=switch action {
    | Create(_) => ""
    | Read(slug) | Update(slug, _) | Delete(slug) => slug
    },
    (),
  )

  let method = switch action {
  | Create(_) => #POST
  | Read(_) => #GET
  | Update(_) => #PUT
  | Delete(_) => #DELETE
  }

  let headers =
    switch action {
    | Create(_) | Update(_) => addJsonContentType()
    | Read(_) | Delete(_) => []
    }
    ->Array.concat(addJwtAuthorization())
    ->Headers.Init.array
    ->Headers.make

  let body = switch action {
  | Create(article) | Update(_, article) =>
    let article =
      [
        ("title", JSON.Encode.string(article.title)),
        ("description", JSON.Encode.string(article.description)),
        ("body", JSON.Encode.string(article.body)),
        ("tagList", JSON.Encode.stringArray(article.tagList)),
      ]
      ->Dict.fromArray
      ->JSON.Encode.object

    [("article", article)]->Dict.fromArray->JSON.Encode.object->JSON.stringify->Body.string
  | Read(_) | Delete(_) => Body.none
  }

  let res = await fetch(
    url,
    {
      method,
      headers,
      body,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractJsonError(res)

  res->Result.flatMap(json => {
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("article")
      ->Option.getOrThrow
      ->Shape.Article.decode
      ->AppError.decode
    } catch {
    | _ => AppError.decode(Error("API.article: failed to decode json"))
    }
  })
}

let favoriteArticle = async (~action: Action.favorite, ()): result<Shape.Article.t, AppError.t> => {
  let url = Endpoints.Articles.favorite(
    ~slug=switch action {
    | Favorite(slug) => slug
    | Unfavorite(slug) => slug
    },
    (),
  )

  let method = switch action {
  | Favorite(_slug) => #POST
  | Unfavorite(_slug) => #DELETE
  }

  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      method,
      headers,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json =>
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("article")
      ->Option.getOrThrow
      ->Shape.Article.decode
      ->AppError.decode
    } catch {
    | _ => AppError.decode(Error("API.favoriteArticle: failed to decode json"))
    }
  )
}

let listArticles = async (
  ~limit: int=10,
  ~offset: int=0,
  ~tag: option<string>=?,
  ~author: option<string>=?,
  ~favorited: option<string>=?,
  (),
): result<Shape.Articles.t, AppError.t> => {
  let url = Endpoints.Articles.root(~limit, ~offset, ~tag?, ~author?, ~favorited?, ())
  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      headers: headers,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => json->Shape.Articles.decode->AppError.decode)
}

let feedArticles = async (~limit: int=10, ~offset: int=0, ()): result<
  Shape.Articles.t,
  AppError.t,
> => {
  let url = Endpoints.Articles.feed(~limit, ~offset, ())
  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      headers: headers,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => json->Shape.Articles.decode->AppError.decode)
}

let tags = async (): result<Shape.Tags.t, AppError.t> => {
  let url = Endpoints.tags()
  let method = #GET

  let res = await fetch(
    url,
    {
      method: method,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => json->Shape.Tags.decode->AppError.decode)
}

let currentUser = async (): result<Shape.User.t, AppError.t> => {
  let url = Endpoints.user()
  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      headers: headers,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => json->Shape.User.decode->AppError.decode)
}

let updateUser = async (~user: Shape.User.t, ~password: string, ()): result<
  Shape.User.t,
  AppError.t,
> => {
  let url = Endpoints.user()

  let user =
    list{
      list{("email", JSON.Encode.string(user.email))},
      list{("bio", JSON.Encode.string(user.bio->Option.getOr("")))},
      list{("image", JSON.Encode.string(user.image->Option.getOr("")))},
      list{("username", JSON.Encode.string(user.username))},
      if password == "" {
        list{}
      } else {
        list{("password", JSON.Encode.string(password))}
      },
    }
    ->List.flat
    ->List.toArray
    ->Dict.fromArray
    ->JSON.Encode.object

  let method = #PUT

  let headers =
    addJwtAuthorization()->Array.concat(addJsonContentType())->Headers.Init.array->Headers.make

  let body = [("user", user)]->Dict.fromArray->JSON.Encode.object->JSON.stringify->Body.string

  let res = await fetch(
    url,
    {
      method,
      headers,
      body,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractJsonError(res)

  res->Result.flatMap(json => json->Shape.User.decode->AppError.decode)
}

let followUser = async (~action: Action.follow, ()): result<Shape.Author.t, AppError.t> => {
  let url = Endpoints.Profiles.follow(
    ~username=switch action {
    | Follow(username) | Unfollow(username) => username
    },
    (),
  )

  let method = switch action {
  | Follow(_username) => #POST
  | Unfollow(_username) => #DELETE
  }

  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      method,
      headers,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => {
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("profile")
      ->Option.getOrThrow
      ->Shape.Author.decode
      ->AppError.decode
    } catch {
    | _ => AppError.decode(Result.Error("API.followUser: failed to decode json"))
    }
  })
}

let getComments = async (~slug: string, ()): result<array<Shape.Comment.t>, AppError.t> => {
  let url = Endpoints.Articles.comments(~slug, ())

  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      headers: headers,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => json->Shape.Comment.decode->AppError.decode)
}

let deleteComment = async (~slug: string, ~id: int, ()): result<(string, int), AppError.t> => {
  let url = Endpoints.Articles.comment(~slug, ~id, ())
  let method = #DELETE
  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      method,
      headers,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(_json => Result.Ok((slug, id)))
}

let addComment = async (~slug: string, ~body: string, ()): result<Shape.Comment.t, AppError.t> => {
  let url = Endpoints.Articles.comments(~slug, ())

  let method = #POST

  let headers =
    addJwtAuthorization()->Array.concat(addJsonContentType())->Headers.Init.array->Headers.make

  let comment = [("body", JSON.Encode.string(body))]->Dict.fromArray->JSON.Encode.object

  let body =
    [("comment", comment)]->Dict.fromArray->JSON.Encode.object->JSON.stringify->Body.string

  let res = await fetch(
    url,
    {
      method,
      headers,
      body,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => {
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("comment")
      ->Option.getOrThrow
      ->Shape.Comment.decodeComment
      ->AppError.decode
    } catch {
    | _ => AppError.decode(Result.Error("API.addComment: failed to decode json"))
    }
  })
}

let getProfile = async (~username: string, ()): result<Shape.Author.t, AppError.t> => {
  let url = Endpoints.Profiles.profile(~username, ())

  let headers = addJwtAuthorization()->Headers.Init.array->Headers.make

  let res = await fetch(
    url,
    {
      headers: headers,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractTextError(res)

  res->Result.flatMap(json => {
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("profile")
      ->Option.getOrThrow
      ->Shape.Author.decode
      ->AppError.decode
    } catch {
    | _ => AppError.decode(Result.Error("API.getProfile: failed to decode json"))
    }
  })
}

let login = async (~email: string, ~password: string, ()): result<Shape.User.t, AppError.t> => {
  let url = Endpoints.Users.login()

  let method = #POST

  let headers = addJsonContentType()->Headers.Init.array->Headers.make

  let user =
    [("email", JSON.Encode.string(email)), ("password", JSON.Encode.string(password))]
    ->Dict.fromArray
    ->JSON.Encode.object

  let body = [("user", user)]->Dict.fromArray->JSON.Encode.object->JSON.stringify->Body.string

  let res = await fetch(
    url,
    {
      method,
      headers,
      body,
    },
  )
  let res = await parseBodyAsJson(res)
  let res = await extractJsonError(res)

  res->Result.flatMap(json => json->Shape.User.decode->AppError.decode)
}

let register = async (~username: string, ~email: string, ~password: string, ()): result<
  Shape.User.t,
  AppError.t,
> => {
  let url = Endpoints.Users.root()

  let method = #POST

  let headers = addJsonContentType()->Headers.Init.array->Headers.make

  let user =
    [
      ("email", JSON.Encode.string(email)),
      ("password", JSON.Encode.string(password)),
      ("username", JSON.Encode.string(username)),
    ]
    ->Dict.fromArray
    ->JSON.Encode.object

  let body = [("user", user)]->Dict.fromArray->JSON.Encode.object->JSON.stringify->Body.string

  let res = await fetch(
    url,
    {
      method,
      headers,
      body,
    },
  )

  let res = await parseBodyAsJson(res)
  let res = await extractJsonError(res)

  res->Result.flatMap(json => json->Shape.User.decode->AppError.decode)
}
