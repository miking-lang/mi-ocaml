include "mexpr/pprint.mc"

include "ast.mc"

lang OCamlStringPprint = PrettyPrint + OCamlStringAst + ConstPrettyPrint
  sem getConstStringCode indent =
  | COString x -> join ["\"", escapeString x.val, "\""]

  sem getTypeStringCode indent env =
  | TyOString _ -> (env, "string")

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
  sem patPrecedence =
  | PatOpaqueOCamlCon x ->
    if optionIsNone x.arg
    then 100000
    else 2
  sem getPatStringCode indent env =
  | PatOpaqueOCamlCon x ->
    match optionMapAccum (getPatStringCode indent) env x.arg with (env, arg) in
    (env, join [x.content, match arg with Some arg then concat " " arg else ""])
end

lang OCamlListPprint = PrettyPrint + OCamlListAst + ConstPrettyPrint
  sem getConstStringCode indent =
  | CONil _ -> "[]"
  | COCons _ -> "(::)"

  sem typePrecedence =
  | TyOList _ -> 1
  sem getTypeStringCode indent env =
  | TyOList x ->
    match getTypeStringCode indent env x.elem with (env, elem) in
    (env, join ["OList ", elem])

  sem patPrecedence =
  | PatOCons _ -> 0
  sem getPatStringCode indent env =
  | PatOCons x ->
    match getPatStringCode indent env x.head with (env, head) in
    match getPatStringCode indent env x.tail with (env, tail) in
    (env, join [head, " :: ", tail])
  | PatONil x -> (env, "[]")
end

lang OCamlCmpPprint = PrettyPrint + OCamlCmpAst + OverloadedOpPrettyPrint
  sem getOpStringCode indent env =
  | OpEq _ -> (env, "=")
  | OpNeq _ -> (env, "!=")
  | OpLt _ -> (env, "<")
  | OpGt _ -> (env, ">")
  | OpLeq _ -> (env, "<=")
  | OpGeq _ -> (env, ">=")

  sem opIsAtomic =
  | OpEq _ | OpNeq _ | OpLt _ | OpGt _ | OpLeq _ | OpGeq _ -> true
end

lang OCamlExtrasPprint = OCamlStringPprint + OCamlOpaquePprint + OCamlListPprint + OCamlCmpPprint
end
