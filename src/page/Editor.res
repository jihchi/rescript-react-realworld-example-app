let parseTagList = (str: string): array<string> =>
  str->String.split(",")->Array.map(String.trim)->Array.filter(v => String.length(v) > 0)

module Form = {
  @react.component
  let make = (~data, ~setData, ~onSubmit: ((Shape.Article.t, string)) => unit) => {
    let isBusy = data->AsyncResult.isBusy
    let error = data->AsyncResult.getOk->Option.flatMap(((_article, _tagList, error)) => error)

    <>
      {switch error {
      | None => React.null
      | Some(error: Shape.Editor.t) =>
        <ul className="error-messages">
          <ErrorDetails label="title" error=error.title />
          <ErrorDetails label="body" error=error.body />
          <ErrorDetails label="description" error=error.description />
        </ul>
      }}
      <form>
        <fieldset>
          <fieldset className="form-group">
            <input
              type_="text"
              className="form-control form-control-lg"
              placeholder="Article Title"
              disabled=isBusy
              value={data
              ->AsyncResult.getOk
              ->Option.map(((
                article: Shape.Article.t,
                _tagList: string,
                _error: option<Shape.Editor.t>,
              )) => article.title)
              ->Option.getWithDefault("")}
              onChange={event => {
                let title = ReactEvent.Form.target(event)["value"]
                setData(prev =>
                  prev->AsyncResult.map(((
                    article: Shape.Article.t,
                    tagList: string,
                    error: option<Shape.Editor.t>,
                  )) => ({...article, title}, tagList, error))
                )
              }}
            />
          </fieldset>
          <fieldset className="form-group">
            <input
              type_="text"
              className="form-control"
              placeholder="What's this article about?"
              disabled=isBusy
              value={data
              ->AsyncResult.getOk
              ->Option.map(((
                article: Shape.Article.t,
                _tagList: string,
                _error: option<Shape.Editor.t>,
              )) => article.description)
              ->Option.getWithDefault("")}
              onChange={event => {
                let description = ReactEvent.Form.target(event)["value"]
                setData(prev =>
                  prev->AsyncResult.map(((
                    article: Shape.Article.t,
                    tagList: string,
                    error: option<Shape.Editor.t>,
                  )) => ({...article, description}, tagList, error))
                )
              }}
            />
          </fieldset>
          <fieldset className="form-group">
            <textarea
              className="form-control"
              rows=8
              placeholder="Write your article (in markdown)"
              disabled=isBusy
              value={data
              ->AsyncResult.getOk
              ->Option.map(((
                article: Shape.Article.t,
                _tagList: string,
                _error: option<Shape.Editor.t>,
              )) => article.body)
              ->Option.getWithDefault("")}
              onChange={event => {
                let body = ReactEvent.Form.target(event)["value"]
                setData(prev =>
                  prev->AsyncResult.map(((
                    article: Shape.Article.t,
                    tagList: string,
                    error: option<Shape.Editor.t>,
                  )) => ({...article, body}, tagList, error))
                )
              }}
            />
          </fieldset>
          <fieldset className="form-group">
            <input
              type_="text"
              className="form-control"
              placeholder="Enter tags"
              disabled=isBusy
              value={data
              ->AsyncResult.getOk
              ->Option.map(((
                _article: Shape.Article.t,
                tagList: string,
                _error: option<Shape.Editor.t>,
              )) => tagList)
              ->Option.getWithDefault("")}
              onChange={event => {
                let tagList = ReactEvent.Form.target(event)["value"]
                setData(prev =>
                  prev->AsyncResult.map(((
                    article: Shape.Article.t,
                    _tagList: string,
                    error: option<Shape.Editor.t>,
                  )) => (article, tagList, error))
                )
              }}
            />
            <div className="tag-list" />
          </fieldset>
          <button
            className="btn btn-lg pull-xs-right btn-primary"
            type_="button"
            disabled=isBusy
            onClick={event => {
              event->ReactEvent.Mouse.preventDefault
              event->ReactEvent.Mouse.stopPropagation

              if isBusy {
                ignore()
              } else {
                switch data->AsyncResult.getOk {
                | Some((
                    article: Shape.Article.t,
                    tagList: string,
                    _error: option<Shape.Editor.t>,
                  )) =>
                  onSubmit((article, tagList))
                  ignore()
                | None => ignore()
                }
              }
            }}>
            {"Publish Article"->React.string}
          </button>
        </fieldset>
      </form>
    </>
  }
}

module Create = {
  let empty = (
    {
      Shape.Article.slug: "",
      title: "",
      description: "",
      body: "",
      tagList: [],
      createdAt: Js.Date.make(),
      updatedAt: Js.Date.make(),
      favorited: false,
      favoritesCount: 0,
      author: {
        Shape.Author.username: "",
        bio: None,
        image: "",
        following: Some(false),
      },
    },
    "",
    None,
  )

  @react.component
  let make = () => {
    let (article, setArticle) = React.useState(() => AsyncResult.completeOk(empty))

    let handleSumbit = async (~article, ~tagList) => {
      setArticle(AsyncResult.toBusy)

      switch await API.article(~action=Create({...article, tagList: parseTagList(tagList)}), ()) {
      | Ok(ok: Shape.Article.t) =>
        Link.article(~slug=ok.slug)->Link.push
        setArticle(prev => prev->AsyncResult.toIdle)
      | Error(AppError.Fetch((_code, _message, #json(json)))) =>
        try {
          let result =
            json
            ->Js.Json.decodeObject
            ->Option.getExn
            ->Js.Dict.get("errors")
            ->Option.getExn
            ->Shape.Editor.decode
          switch result {
          | Ok(errors) =>
            setArticle(prev =>
              prev
              ->AsyncData.toIdle
              ->AsyncResult.map(((article, tagList, _error)) => (article, tagList, Some(errors)))
            )
          | Error(_e) => ignore()
          }
        } catch {
        | _ => Js.log("Button.UpdateSettings: failed to decode json")
        }
      | Error(Fetch((_, _, #text(_)))) | Error(Decode(_)) => setArticle(AsyncResult.toIdle)
      }
    }

    <Form
      data=article
      setData=setArticle
      onSubmit={((article, tagList)) => {
        handleSumbit(~article, ~tagList)->ignore
      }}
    />
  }
}

module Edit = {
  @react.component
  let make = (~slug: string) => {
    let (article, setArticle) = Hook.useArticle(~slug)

    let handleSubmit = async (~article, ~tagList) => {
      setArticle(AsyncResult.toBusy)
      let _ = await API.article(
        ~action=Update(slug, {...article, tagList: parseTagList(tagList)}),
        (),
      )
      setArticle(AsyncResult.toIdle)
    }

    <Form
      data=article
      setData=setArticle
      onSubmit={((article, tagList)) => {
        handleSubmit(~article, ~tagList)->ignore
      }}
    />
  }
}

@react.component
let make = (~slug: option<string>=?) =>
  <div className="editor-page">
    <div className="container page">
      <div className="row">
        <div className="col-md-10 offset-md-1 col-xs-12">
          {switch slug {
          | Some(slug) => <Edit slug />
          | None => <Create />
          }}
        </div>
      </div>
    </div>
  </div>
