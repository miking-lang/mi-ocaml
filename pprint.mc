include "mexpr/pprint.mc"

include "ast.mc"

lang OCamlStringPprint = PrettyPrint + OCamlStringAst
  sem getConstStringCode indent =
  | COString x -> join ["\"", escapeString x.val, "\""]

  sem getTypeStringCode indent env =
  | TyOString _ -> (env, "string")

  sem patIsAtomic =
  | PatOString _ -> true
  sem getPatStringCode indent env =
  | PatOString x -> (env, join ["\"", escapeString x.val, "\""])
end

lang OCamlOpaquePprint = PrettyPrint + OpaqueOCamlAst
  -- Expr
  sem isAtomic =
  | TmOpaqueOCaml _ -> true
  sem pprintCode indent env =
  | TmOpaqueOCaml x -> (env, x.content)

  -- Type
  sem getTypeStringCode indent env =
  | TyOpaqueOCaml x -> (env, x.content)

  -- Pat
  sem patIsAtomic =
  | PatOpaqueOCamlCon x -> optionIsNone x.arg
  sem getPatStringCode indent env =
  | PatOpaqueOCamlCon x ->
    match optionMapAccum (getPatStringCode indent) env x.arg with (env, arg) in
    (env, join [x.content, match arg with Some arg then concat " " arg else ""])
end

lang OCamlListPprint = PrettyPrint + OCamlListAst
  sem getConstStringCode indent =
  | CONil _ -> "[]"
  | COCons _ -> "(::)"

  sem typePrecedence =
  | TyOList _ -> 1
  sem getTypeStringCode indent env =
  | TyOList x ->
    match getTypeStringCode indent env x.elem with (env, elem) in
    (env, join ["OList ", elem])

  sem patIsAtomic =
  | PatOCons _ -> false
  | PatONil _ -> true
  sem getPatStringCode indent env =
  | PatOCons x ->
    match getPatStringCode indent env x.head with (env, head) in
    match getPatStringCode indent env x.tail with (env, tail) in
    (env, join [head, " :: ", tail])
  | PatONil x -> (env, "[]")
end

lang OCamlExtrasPprint = OCamlStringPprint + OCamlOpaquePprint + OCamlListPprint
end
