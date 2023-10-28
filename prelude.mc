include "mexpr/ast-builder.mc"

lang OCamlPrelude = ReprTypeAst + OCamlListAst + OCamlStringAst
  sem wrapInPrelude = | tm ->
    let info = Info {filename = "<prelude>", row1 = 0, col1 = 0, row2 = 0, col2 = 0} in
    let bindings =
      [ type_ "repr" ["x"] (TyRepr {info = info, arg = tyvar_ "x", repr = ref (UninitRepr ())})
      , type_ "list" ["x"] (TyOList {info = info, elem = tyvar_ "x"})
      , type_ "string" [] (TyOString {info = info})
      , type_ "char" [] tychar_
      , type_ "int" [] tyint_
      , type_ "float" [] tyfloat_
      , type_ "bool" [] tybool_
      ] in
    bindall_ (snoc bindings tm)
end
