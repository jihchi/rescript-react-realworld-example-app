@react.component
let make = (
  ~slug: string,
  ~image: string,
  ~setComments: (
    AsyncResult.t<array<Shape.Comment.t>, AppError.t> => AsyncResult.t<
      array<Shape.Comment.t>,
      AppError.t,
    >
  ) => unit,
) => {
  let (comment, setComment) = React.useState(() => AsyncData.complete(""))
  let body = comment->AsyncData.getValue->Option.getWithDefault("")
  let isCommentValid =
    comment->AsyncData.getValue->Option.map(v => String.trim(v) != "")->Option.getWithDefault(false)

  let handlePostCommentClick = async () => {
    if isCommentValid && AsyncData.isComplete(comment) {
      setComment(AsyncData.toBusy)
      switch await API.addComment(~slug, ~body, ()) {
      | Ok(comment) =>
        setComments(prev =>
          prev->AsyncResult.map(comments => {
            let _ = comments->Array.unshift(comment)
            comments
          })
        )
        setComment(_prev => AsyncData.complete(""))
      | Error(_error) => setComment(AsyncData.toIdle)
      }
    }
  }

  <form className="card comment-form">
    <div className="card-block">
      <textarea
        className="form-control"
        placeholder="Write a comment..."
        rows=3
        value=body
        onChange={event => {
          let value = ReactEvent.Form.target(event)["value"]
          setComment(_prev => AsyncData.complete(value))
        }}
        disabled={comment->AsyncData.isBusy}
      />
    </div>
    <div className="card-footer">
      {switch image {
      | "" => <img className="comment-author-img" />
      | src => <img src className="comment-author-img" />
      }}
      <button
        className="btn btn-sm btn-primary"
        disabled={!isCommentValid}
        onClick={event => {
          event->ReactEvent.Mouse.preventDefault
          event->ReactEvent.Mouse.stopPropagation
          handlePostCommentClick()->ignore
        }}>
        {"Post Comment"->React.string}
      </button>
    </div>
  </form>
}
