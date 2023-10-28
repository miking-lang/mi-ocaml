include "mexpr/ast.mc"

lang OpaqueOCamlAst = Ast
  -- Expr
  syn Expr =
  | TmOpaqueOCaml {info : Info, content : String, ty : Type}

  sem infoTm =
  | TmOpaqueOCaml x -> x.info
  sem withInfo info =
  | TmOpaqueOCaml x -> TmOpaqueOCaml {x with info = info}

  sem tyTm =
  | TmOpaqueOCaml x -> x.ty
  sem withType ty =
  | TmOpaqueOCaml x -> TmOpaqueOCaml {x with ty = ty}

  -- Type
  syn Type =
  | TyOpaqueOCaml {info : Info, content : String}

  sem infoTy =
  | TyOpaqueOCaml x -> x.info
  sem tyWithInfo info =
  | TyOpaqueOCaml x -> TyOpaqueOCaml {x with info = info}

  -- Pat
  syn Pat =
  | PatOpaqueOCamlCon {info : Info, content : String, arg : Option Pat, ty : Type}

  sem infoPat =
  | PatOpaqueOCamlCon x -> x.info
  sem withInfoPat info =
  | PatOpaqueOCamlCon x -> PatOpaqueOCamlCon {x with info = info}

  sem tyPat =
  | PatOpaqueOCamlCon x -> x.ty
  sem withTypePat ty =
  | PatOpaqueOCamlCon x -> PatOpaqueOCamlCon {x with ty = ty}

  sem smapAccumL_Pat_Pat f acc =
  | PatOpaqueOCamlCon x ->
    match optionMapAccum f acc x.arg with (acc, arg) in
    (acc, PatOpaqueOCamlCon {x with arg = arg})
end

lang OCamlStringAst = Ast
  -- Constant
  syn Const =
  | COString {val : String}

  sem tyConst =
  | COString _ -> TyOString {info = NoInfo ()}

  -- Type
  syn Type =
  | TyOString {info : Info}

  sem infoTy =
  | TyOString x -> x.info
  sem tyWithInfo info =
  | TyOString x -> TyOString {x with info = info}

  -- Pattern
  syn Pat =
  | PatOString {info : Info, ty : Type, val : String}

  sem infoPat =
  | PatOString x -> x.info
  sem withInfoPat info =
  | PatOString x -> PatOString {x with info = info}

  sem tyPat =
  | PatOString x -> x.ty
  sem withTypePat ty =
  | PatOString x -> PatOString {x with ty = ty}
end

lang OCamlListAst = Ast
  -- Constant
  syn Const =
  | CONil ()
  | COCons ()

  sem tyConst =
  | CONil _ -> tyall_ "x"
    (TyOList {info = NoInfo (), elem = tyvar_ "x"})
  | COCons _ -> tyall_ "x" (tyarrows_
    [ tyvar_ "x"
    , TyOList {info = NoInfo (), elem = tyvar_ "x"}
    , TyOList {info = NoInfo (), elem = tyvar_ "x"}
    ])

  -- Type
  syn Type =
  | TyOList {info : Info, elem : Type}

  sem infoTy =
  | TyOList x -> x.info
  sem tyWithInfo info =
  | TyOList x -> TyOList {x with info = info}

  sem smapAccumL_Type_Type f acc =
  | TyOList x ->
    match f acc x.elem with (acc, elem) in
    (acc, TyOList {x with elem = elem})

  -- Pattern
  syn Pat =
  | PatOCons {info : Info, ty : Type, head : Pat, tail : Pat}
  | PatONil {info : Info, ty : Type}

  sem smapAccumL_Pat_Pat f acc =
  | PatOCons x ->
    match f acc x.head with (acc, head) in
    match f acc x.tail with (acc, tail) in
    (acc, PatOCons {x with head = head, tail = tail})

  sem infoPat =
  | PatOCons x -> x.info
  | PatONil x -> x.info
  sem withInfoPat info =
  | PatOCons x -> PatOCons {x with info = info}
  | PatONil x -> PatONil {x with info = info}

  sem tyPat =
  | PatOCons x -> x.ty
  | PatONil x -> x.ty
  sem withTypePat ty =
  | PatOCons x -> PatOCons {x with ty = ty}
  | PatONil x -> PatONil {x with ty = ty}
end
