include "mexpr/shallow-patterns.mc"
include "ast.mc"

lang ShallowOpaqueOCaml = ShallowBase + OpaqueOCamlAst
  syn SPat =
  | SPatOpaqueOCaml {content : String, subName : Option Name}

  sem decompose name =
  | (SPatOpaqueOCaml shallow, pat & PatOpaqueOCamlCon x) ->
    if eqString shallow.content x.content then
      switch (x.arg, shallow.subName)
      case (Some arg, Some subName) then
        ([(_singleton subName arg, _empty ())], None ())
      case (None _, None _) then
        ([(_empty (), _empty ())], None ())
      case _ then
        defaultDecomposition pat
      end
    else defaultDecomposition pat

  sem collectShallows =
  | PatOpaqueOCamlCon x -> _ssingleton (SPatOpaqueOCaml {content = x.content, subName = optionMap (lam. nameSym "carried") x.arg})

  sem mkMatch scrutinee t e =
  | SPatOpaqueOCaml x ->
    let pat = PatOpaqueOCamlCon {info = NoInfo (), content = x.content, arg = optionMap npvar_ x.subName, ty = tyunknown_} in
    match_ (nvar_ scrutinee) pat t e

  sem shallowCmp =
  | (SPatOpaqueOCaml l, SPatOpaqueOCaml r) ->
    let res = cmpString l.content r.content in
    if neqi res 0 then res else
    let res = subi (constructorTag l.subName) (constructorTag r.subName) in
    if neqi res 0 then res else
    match (l.subName, r.subName) with (Some l, Some r)
    then nameCmp l r
    else 0
end

lang ShallowOCamlString = ShallowBase + OCamlStringAst
  syn SPat =
  | SPatOString String

  sem decompose name =
  | (SPatOString str, pat & PatOString x) ->
    if eqString str x.val
    then ([(_empty (), _empty ())], None ())
    else defaultDecomposition pat

  sem collectShallows =
  | PatOString x -> _ssingleton (SPatOString x.val)

  sem mkMatch scrutinee t e =
  | SPatOString str ->
    let pat = PatOString {info = NoInfo (), val = str, ty = tyunknown_} in
    match_ (nvar_ scrutinee) pat t e

  sem shallowCmp =
  | (SPatOString l, SPatOString r) -> cmpString l r
end

lang ShallowOCamlList = ShallowBase + OCamlListAst
  syn SPat =
  | SPatOCons {head : Name, tail : Name}
  | SPatONil ()

  sem decompose name =
  | (SPatOCons shallow, PatOCons x) ->
    ([(mapInsert shallow.head x.head (_singleton shallow.tail x.tail), _empty ())], None ())
  | (SPatONil shallow, PatONil _) ->
    ([(_empty (), _empty ())], None ())

  sem collectShallows =
  | PatOCons x -> _ssingleton (SPatOCons {head = nameSym "head", tail = nameSym "tail"})
  | PatONil _ -> _ssingleton (SPatONil ())

  sem mkMatch scrutinee t e =
  | SPatOCons x ->
    let pat = PatOCons {head = npvar_ x.head, tail = npvar_ x.tail, info = NoInfo (), ty = tyunknown_} in
    match_ (nvar_ scrutinee) pat t e

  sem shallowCmp =
  | (SPatOCons l, SPatOCons r) ->
    let res = nameCmp l.head r.head in
    if neqi 0 res then res else
    nameCmp l.tail r.tail
end

lang ShallowOCamlExtras = ShallowOpaqueOCaml + ShallowOCamlString + ShallowOCamlList
end
