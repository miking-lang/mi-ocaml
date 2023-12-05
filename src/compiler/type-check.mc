include "mexpr/type-check.mc"

lang OpaqueOCamlTypeCheck = TypeCheck + OpaqueOCamlAst + CompatibleType + TypeAnnot + MatchTypeAnnot + PatTypeCheck
  sem typeCheckExpr env =
  | TmOpaqueOCaml x ->
    TmOpaqueOCaml {x with ty = newmonovar env.currentLvl x.info}

  sem unifyBase u env =
  | (ty1 & TyOpaqueOCaml a, ty2 & TyOpaqueOCaml b) ->
    if eqString a.content b.content
    then u.empty
    else u.err (Types (ty1, ty2))

  sem typeCheckPat env patEnv =
  | PatOpaqueOCamlCon x ->
    match optionMapAccum (typeCheckPat env) patEnv x.arg with (patEnv, arg) in
    (patEnv, PatOpaqueOCamlCon {x with ty = newmonovar env.currentLvl x.info, arg = arg})

  sem compatibleTypeBase tyEnv =
  | (ty1 & TyOpaqueOCaml a, TyOpaqueOCaml b) ->
    if eqString a.content b.content
    then Some ty1
    else None ()

  sem typeAnnotExpr env =
  | tm & TmOpaqueOCaml _ -> tm

  sem typeAnnotPat env expectedTy =
  | pat & PatOpaqueOCamlCon _ -> (env, pat)
end

lang OCamlStringTypeCheck = TypeCheck + Unify + OCamlStringAst + CompatibleType + MatchTypeAnnot + PatTypeCheck
  sem typeCheckPat env patEnv =
  | PatOString x ->
    (patEnv, PatOString {x with ty = TyOString {info = x.info}})

  sem unifyBase u env =
  | (TyOString _, TyOString _) -> u.empty

  sem compatibleTypeBase tyEnv =
  | (ty1 & TyOString _, TyOString _) -> Some ty1

  sem typeAnnotPat env expectedTy =
  | pat & PatOString _ -> (env, pat)
end

lang OCamlListTypeCheck = TypeCheck + Unify + OCamlListAst + CompatibleType + MatchTypeAnnot + PatTypeCheck
  sem typeCheckPat env patEnv =
  | PatOCons x ->
    match typeCheckPat env patEnv x.head with (patEnv, head) in
    match typeCheckPat env patEnv x.tail with (patEnv, tail) in
    let ty = TyOList {info = x.info, elem = tyPat head} in
    unify env [infoPat head, infoPat tail] ty (tyPat tail);
    (patEnv, PatOCons {x with ty = ty, head = head, tail = tail})
  | PatONil x ->
    let ty = TyOList {info = x.info, elem = newmonovar env.currentLvl x.info} in
    (patEnv, PatONil {x with ty = ty})

  sem unifyBase u env =
  | (TyOList a, TyOList b) ->
    unifyTypes u env (a.elem, b.elem)

  sem compatibleTypeBase tyEnv =
  | (TyOList a, TyOList b) ->
    optionMap (lam elem. TyOList {a with elem = elem}) (compatibleType tyEnv a.elem b.elem)

  sem typeAnnotPat env expectedTy =
  | pat & PatOCons _ -> (env, pat)
  | pat & PatONil _ -> (env, pat)
end

lang OCamlExtrasTypeCheck = OpaqueOCamlTypeCheck + OCamlStringTypeCheck + OCamlListTypeCheck
end
