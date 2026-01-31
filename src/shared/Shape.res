module Json = Js.Json

type decodeError = string

module Profile = {
  type username = string
  type limit = int
  type offset = int
  type viewMode =
    | Author(username, limit, offset)
    | Favorited(username, limit, offset)
}

module FeedType = {
  type tag = string
  type limit = int
  type offset = int
  type t =
    | Tag(tag, limit, offset)
    | Global(limit, offset)
    | Personal(limit, offset)
}

module Author = {
  type t = {
    username: string,
    bio: option<string>,
    image: string,
    following: option<bool>,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let username =
        obj->Dict.get("username")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let bio = obj->Dict.get("bio")->Option.flatMap(JSON.Decode.string)
      let image = obj->Dict.get("image")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let following = obj->Dict.get("following")->Option.flatMap(JSON.Decode.bool)

      Result.Ok({
        username,
        bio,
        image,
        following,
      })
    } catch {
    | _ => Error("Shape.Author: failed to decode json")
    }
  }
}

module Article = {
  type t = {
    slug: string,
    title: string,
    description: string,
    body: string,
    tagList: array<string>,
    createdAt: Date.t,
    updatedAt: Date.t,
    favorited: bool,
    favoritesCount: int,
    author: Author.t,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let slug = obj->Dict.get("slug")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let title = obj->Dict.get("title")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let description =
        obj->Dict.get("description")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let body = obj->Dict.get("body")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let tagList =
        obj
        ->Dict.get("tagList")
        ->Option.flatMap(JSON.Decode.array)
        ->Option.flatMap(tagList => Some(tagList->Array.filterMap(JSON.Decode.string)))
        ->Option.getOrThrow
      let createdAt =
        obj
        ->Dict.get("createdAt")
        ->Option.flatMap(JSON.Decode.string)
        ->Option.getOrThrow
        ->Date.fromString
      let updatedAt =
        obj
        ->Dict.get("updatedAt")
        ->Option.flatMap(JSON.Decode.string)
        ->Option.getOrThrow
        ->Date.fromString
      let favorited =
        obj->Dict.get("favorited")->Option.flatMap(JSON.Decode.bool)->Option.getOrThrow
      let favoritesCount =
        obj
        ->Dict.get("favoritesCount")
        ->Option.flatMap(JSON.Decode.float)
        ->Option.getOrThrow
        ->Float.toInt
      let author =
        obj
        ->Dict.get("author")
        ->Option.flatMap(author => {
          switch author->Author.decode {
          | Ok(ok) => Some(ok)
          | Error(_err) => None
          }
        })
        ->Option.getOrThrow

      Result.Ok({
        slug,
        title,
        description,
        body,
        tagList,
        createdAt,
        updatedAt,
        favorited,
        favoritesCount,
        author,
      })
    } catch {
    | _ => Error("Shape.Article: failed to decode json")
    }
  }
}

module Articles = {
  type t = {
    articles: array<Article.t>,
    articlesCount: int,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let articles =
        obj
        ->Dict.get("articles")
        ->Option.flatMap(JSON.Decode.array)
        ->Option.flatMap(articles => {
          articles
          ->Array.filterMap(article =>
            switch article->Article.decode {
            | Ok(ok) => Some(ok)
            | Error(_err) => None
            }
          )
          ->Some
        })
        ->Option.getOrThrow
      let articlesCount =
        obj
        ->Dict.get("articlesCount")
        ->Option.flatMap(JSON.Decode.float)
        ->Option.map(Float.toInt)
        ->Option.getOrThrow

      Result.Ok({
        articles,
        articlesCount,
      })
    } catch {
    | _ => Error("Shape.Article: failed to decode json")
    }
  }
}

module Tags = {
  type t = array<string>

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let tags =
        obj
        ->Dict.get("tags")
        ->Option.flatMap(JSON.Decode.array)
        ->Option.map(tags => tags->Array.filterMap(JSON.Decode.string))
        ->Option.getOrThrow

      Result.Ok(tags)
    } catch {
    | _ => Error("Shape.Tags: failed to decode json")
    }
  }
}

module User = {
  type t = {
    email: string,
    username: string,
    bio: option<string>,
    image: option<string>,
    token: string,
  }

  let empty = {
    email: "",
    username: "",
    bio: None,
    image: None,
    token: "",
  }

  let decodeUser = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let email = obj->Dict.get("email")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let username =
        obj->Dict.get("username")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let bio = obj->Dict.get("bio")->Option.flatMap(JSON.Decode.string)
      let image = obj->Dict.get("image")->Option.flatMap(JSON.Decode.string)
      let token = obj->Dict.get("token")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow

      Result.Ok({
        email,
        username,
        bio,
        image,
        token,
      })
    } catch {
    | _ => Error("Shape.User: failed to decode json")
    }
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let user =
        obj
        ->Dict.get("user")
        ->Option.flatMap(user => {
          switch user->decodeUser {
          | Ok(ok) => Some(ok)
          | Error(_err) => None
          }
        })
        ->Option.getOrThrow

      Result.Ok(user)
    } catch {
    | _ => Error("Shape.User: failed to decode json")
    }
  }
}

module CommentUser = {
  type t = {
    username: string,
    bio: option<string>,
    image: string,
    following: bool,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let username =
        obj->Dict.get("username")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let bio = obj->Dict.get("bio")->Option.flatMap(JSON.Decode.string)
      let image = obj->Dict.get("image")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let following =
        obj->Dict.get("following")->Option.flatMap(JSON.Decode.bool)->Option.getOrThrow

      Result.Ok({
        username,
        bio,
        image,
        following,
      })
    } catch {
    | _ => Error("Shape.CommentUser: failed to decode json")
    }
  }
}

module Comment = {
  type t = {
    id: int,
    createdAt: Date.t,
    updatedAt: Date.t,
    body: string,
    author: CommentUser.t,
  }

  let decodeComment = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let id =
        obj
        ->Dict.get("id")
        ->Option.flatMap(JSON.Decode.float)
        ->Option.map(Float.toInt)
        ->Option.getOrThrow
      let createdAt =
        obj
        ->Dict.get("createdAt")
        ->Option.flatMap(JSON.Decode.string)
        ->Option.map(Date.fromString)
        ->Option.getOrThrow
      let updatedAt =
        obj
        ->Dict.get("updatedAt")
        ->Option.flatMap(JSON.Decode.string)
        ->Option.map(Date.fromString)
        ->Option.getOrThrow
      let body = obj->Dict.get("body")->Option.flatMap(JSON.Decode.string)->Option.getOrThrow
      let author =
        obj
        ->Dict.get("author")
        ->Option.flatMap(author => {
          switch author->CommentUser.decode {
          | Ok(ok) => Some(ok)
          | Error(_err) => None
          }
        })
        ->Option.getOrThrow

      Result.Ok({
        id,
        createdAt,
        updatedAt,
        body,
        author,
      })
    } catch {
    | _ => Error("Shape.Comment: failed to decode json")
    }
  }

  let decode = (json: JSON.t): Result.t<array<t>, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let comments =
        obj
        ->Dict.get("comments")
        ->Option.flatMap(JSON.Decode.array)
        ->Option.map(comments => {
          comments->Array.filterMap(comment => {
            switch comment->decodeComment {
            | Ok(ok) => Some(ok)
            | Error(_err) => None
            }
          })
        })
        ->Option.getOrThrow

      Result.Ok(comments)
    } catch {
    | _ => Error("Shape.Comment: failed to decode json")
    }
  }
}

module Settings = {
  type t = {
    email: option<array<string>>,
    bio: option<array<string>>,
    image: option<array<string>>,
    username: option<array<string>>,
    password: option<array<string>>,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let email = obj->Dict.get("email")->Utils.Json.decodeArrayString
      let bio = obj->Dict.get("bio")->Utils.Json.decodeArrayString
      let image = obj->Dict.get("image")->Utils.Json.decodeArrayString
      let username = obj->Dict.get("username")->Utils.Json.decodeArrayString
      let password = obj->Dict.get("password")->Utils.Json.decodeArrayString

      Result.Ok({
        email,
        bio,
        image,
        username,
        password,
      })
    } catch {
    | _ => Error("Shape.Settings: failed to decode json")
    }
  }
}

module Editor = {
  type t = {
    title: option<array<string>>,
    body: option<array<string>>,
    description: option<array<string>>,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let title = obj->Dict.get("title")->Utils.Json.decodeArrayString
      let body = obj->Dict.get("body")->Utils.Json.decodeArrayString
      let description = obj->Dict.get("description")->Utils.Json.decodeArrayString

      Result.Ok({
        title,
        body,
        description,
      })
    } catch {
    | _ => Error("Shape.Editor: failed to decode json")
    }
  }
}

module Login = {
  type t = option<array<string>>

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      json
      ->JSON.Decode.object
      ->Option.getOrThrow
      ->Dict.get("email or password")
      ->Utils.Json.decodeArrayString
      ->Ok
    } catch {
    | _ => Error("Shape.Login: failed to decode json")
    }
  }
}

module Register = {
  type t = {
    email: option<array<string>>,
    password: option<array<string>>,
    username: option<array<string>>,
  }

  let decode = (json: JSON.t): Result.t<t, decodeError> => {
    try {
      let obj = json->JSON.Decode.object->Option.getOrThrow
      let email = obj->Dict.get("email")->Utils.Json.decodeArrayString
      let username = obj->Dict.get("username")->Utils.Json.decodeArrayString
      let password = obj->Dict.get("password")->Utils.Json.decodeArrayString

      Result.Ok({
        email,
        password,
        username,
      })
    } catch {
    | _ => Error("Shape.Register: failed to decode json")
    }
  }
}
