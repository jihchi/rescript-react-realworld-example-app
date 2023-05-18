module DomPurify = {
  type t
  @module("dompurify") external make: Dom.window => t = "default"
  @send external sanitize: (t, string) => string = "sanitize"
}

module Marked = {
  type options = {
    mangle: bool,
    headerIds: bool,
  }
  @module("marked") @scope("marked") external parse: string => string = "parse"
  @module("marked") external use: options => unit = "use"
}

Marked.use({mangle: false, headerIds: false})

let dompurify = DomPurify.make(Webapi.Dom.window)

let toHTML = (markdown: string): string => {
  dompurify->DomPurify.sanitize(Marked.parse(markdown))
}
