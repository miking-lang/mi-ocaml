include "mexpr/cmp.mc"
include "ast.mc"

lang OpaqueOCamlCmp = Cmp + OpaqueOCamlAst
  sem cmpTypeH =
  | (TyOpaqueOCaml a, TyOpaqueOCaml b) ->
    cmpString a.content b.content
end

lang OCamlStringCmp = Cmp + OCamlStringAst
  sem cmpTypeH =
  | (TyOString _, TyOString _) -> 0
end

lang OCamlListCmp = Cmp + OCamlListAst
  sem cmpTypeH =
  | (TyOList a, TyOList b) ->
    cmpType a.elem b.elem
end

lang OCamlExtrasCmp = OpaqueOCamlCmp + OCamlStringCmp + OCamlListCmp
end
