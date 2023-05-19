@react.component
let make = (~limit: int, ~offset: int, ~total: int, ~onClick: int => unit) =>
  if total == 0 {
    React.null
  } else {
    let pages = Float.toInt(Math.ceil(Int.toFloat(total) /. Int.toFloat(limit))) - 1

    <WithTestId id="page-link">
      <ul className="pagination">
        {Array.make(~length=pages, ())
        ->Array.mapWithIndex((_unit, page) => {
          let className = if (offset == 0 && page == 0) || page == offset / limit {
            "page-item active"
          } else {
            "page-item"
          }

          <li key={page->string_of_int} className>
            <a
              className="page-link"
              href={`#${page->Int.toString}`}
              onClick={event =>
                if Utils.isMouseRightClick(event) {
                  event->ReactEvent.Mouse.preventDefault
                  onClick(page * limit)
                }}>
              {string_of_int(page + 1)->React.string}
            </a>
          </li>
        })
        ->React.array}
      </ul>
    </WithTestId>
  }
