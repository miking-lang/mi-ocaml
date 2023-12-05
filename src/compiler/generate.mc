include "ocaml/generate.mc"
include "ast.mc"

lang OpaqueOCamlGenerate = OCamlGenerate + OpaqueOCamlAst
  sem generate env =
  | TmOpaqueOCaml x -> OTmVarExt {ident = x.content}
  | TmMatch (t & {pat = PatOpaqueOCamlCon _, target = TmVar _}) ->
    let mkExternal = lam pat.
      match pat with PatOpaqueOCamlCon x in
      OPatConExt {ident = x.content, args = optionMapOr [] (lam x. [x]) x.arg} in
    match
      collectNestedMatches env
        (lam pat. match pat with PatOpaqueOCamlCon _ then true else false) []
        (lam acc. lam t. snoc acc (mkExternal t.pat, generate env t.thn)) t
    with (arms, defaultCase) in
    _omatch_ (objMagic (generate env t.target))
      (snoc arms (pvarw_, generate env defaultCase))
end

lang OCamlStringGenerate = OCamlGenerate + OCamlStringAst
  sem generate env =
  | TmConst {val = COString x} -> OTmVarExt {ident = join ["\"", escapeString x.val, "\""]}
  | TmMatch (t & {pat = PatOString _, target = TmVar _}) ->
    let mkExternal = lam pat.
      match pat with PatOString x in
      OPatConExt {ident = join ["\"", escapeString x.val, "\""], args = []} in
    match
      collectNestedMatches env
        (lam pat. match pat with PatOString _ then true else false) []
        (lam acc. lam t. snoc acc (mkExternal t.pat, generate env t.thn)) t
    with (arms, defaultCase) in
    _omatch_ (objMagic (generate env t.target))
      (snoc arms (pvarw_, generate env defaultCase))
end

lang OCamlMatchGenerateOList = OCamlMatchGenerate + OCamlListAst
  sem generate env =
  | TmConst {val = CONil _} -> OTmVarExt {ident = "[]"}
  | TmApp
    { lhs = TmApp
      { lhs = TmConst {val = COCons _}
      , rhs = head
      }
    , rhs = tail
    } -> OTmConAppExt {ident = "(::)", args = [generate env head, generate env tail]}
  | TmMatch (t & {pat = PatOCons _ | PatONil _, target = TmVar _}) ->
    let mkExternal = lam pat. switch pat
      case PatOCons x then
        OPatConExt {ident = "(::)", args = [x.head, x.tail]}
      case PatONil _ then
        OPatConExt {ident = "[]", args = []}
      end in
    match
      collectNestedMatches env
        (lam pat. match pat with PatOCons _ | PatONil _ then true else false) []
        (lam acc. lam t. snoc acc (mkExternal t.pat, generate env t.thn)) t
    with (arms, defaultCase) in
    _omatch_ (objMagic (generate env t.target))
      (snoc arms (pvarw_, generate env defaultCase))
end

lang OCamlExtrasGenerate = OpaqueOCamlGenerate + OCamlStringGenerate + OCamlMatchGenerateOList
end
