include "mexpr/ast.mc"
include "tuning/ast.mc"

include "syntax.mc"
include "ast.mc"
include "cost.mc"

lang ConvertOCamlToMExpr = Ast + OCamlBaseAst
  sem ocamlToMExpr : [OTop] -> Expr
  sem ocamlToMExpr = | tops ->
    foldr (lam t. lam cont. convTop cont t) unit_ tops

  sem convTop : Expr -> OTop -> Expr
  sem convTop cont =
  | t -> errorSingle [get_OTop_info t] "Missing case for convTop"

  sem convTyBinding : OTyBinding -> {tyDef : Expr -> Expr, conDefs : Expr -> Expr}
  sem convTyBinding =
  | t -> errorSingle [get_OTyBinding_info t] "Missing case for convTyBinding"

  sem convParam : OParam -> (Pat, Type)
  sem convParam =
  | t -> errorSingle [get_OParam_info t] "Missing case for convParam"

  sem convBinding : OBinding -> (Name, [(Pat, Type)], Type, Expr)
  sem convBinding =
  | t -> errorSingle [get_OBinding_info t] "Missing case for convBinding"

  sem convExpr : OExpr -> Expr
  sem convExpr =
  | t -> errorSingle [get_OExpr_info t] "Missing case for convExpr"

  sem convPat : OPat -> Pat
  sem convPat =
  | t -> errorSingle [get_OPat_info t] "Missing case for convPat"

  sem convType : OType -> Type
  sem convType =
  | t -> errorSingle [get_OType_info t] "Missing case for convType"
end

-- Helpers --

lang MkBody = LamAst + NamedPat
  sem mkBody : [Pat] -> Expr -> Expr
  sem mkBody params = | body -> foldr
    (lam pat. lam body.
      let info = infoPat pat in
      match switch pat
        case PatNamed {ident = PName ident} then
          (ident, body)
        case PatNamed {ident = PWildcard _} then
          (nameSym "x", body)
        case pat then
          let ident = nameSym "x" in
          (ident, match_ (nvar_ ident) pat body (withInfo info never_))
        end
      with (ident, body) in
      TmLam
      { ident = ident
      , tyAnnot = tyunknown_
      , tyParam = tyunknown_
      , body = body
      , ty = tyunknown_
      , info = mergeInfo info (infoTm body)
      })
    body
    params
end

lang FindVars = VarTypeAst
  sem findVars : Set Name -> Type -> Set Name
  sem findVars acc =
  | ty -> sfold_Type_Type findVars acc ty
  | TyVar x -> setInsert x.ident acc
end

lang MkFuncType = FindVars
  sem mkFuncType params = | retTy ->
    let ty = foldr (lam argTy. lam acc. tyarrow_ argTy acc) retTy params in
    let vars = findVars (setEmpty nameCmp) ty in
    foldr ntyall_ ty (setToSeq vars)
end

lang MkLet = ConvertOCamlToMExpr + DeclAst + RecLetsDeclAst + LetDeclAst + MkBody + FindVars + MkFuncType
  sem mkLet : Info -> Option Info -> [Unknown] -> Expr -> Expr
  sem mkLet info rec bindings = | inexpr ->
    match rec with Some _ then
      let mkRecBinding = lam bind.
        let bind = convBinding bind in
        let body = mkBody (map (lam x. x.0) bind.1) bind.3 in
        { ident = bind.0
        , tyAnnot = mkFuncType (map (lam x. x.1) bind.1) bind.2
        , tyBody = tyunknown_
        , body = body
        , info = infoTm body
        } in
      TmDecl
      { decl = DeclRecLets
        { bindings = map mkRecBinding bindings
        , info = info
        }
      , info = info
      , ty = tyunknown_
      , inexpr = inexpr
      }
    else foldr
      (lam bind. lam cont.
        let bind = convBinding bind in TmDecl
        { decl = DeclLet
          { ident = bind.0
          , tyAnnot = mkFuncType (map (lam x. x.1) bind.1) bind.2
          , tyBody = tyunknown_
          , body = mkBody (map (lam x. x.0) bind.1) bind.3
          , info = info
          }
        , info = info
        , inexpr = cont
        , ty = tyunknown_
        })
      inexpr
      bindings
end

-- Tops --

lang ConvertLetOTop = ConvertOCamlToMExpr + LetOTopAst + MkLet
  sem convTop cont =
  | LetOTop x ->
    mkLet x.info x.rec x.bindings cont
end

lang ConvertTypeOTop = ConvertOCamlToMExpr + TypeOTopAst
  sem convTop cont =
  | TypeOTop x ->
    let bindings = map convTyBinding x.bindings in
    let cont = foldr (lam f. lam acc. f.conDefs acc) cont bindings in
    let cont = foldr (lam f. lam acc. f.tyDef acc) cont bindings in
    withInfo x.info cont
end

lang ConvertLetOpOTop = ConvertOCamlToMExpr + LetOpOTopAst + OpDeclAst + FindVars + DeclAst
  sem convTop cont =
  | LetOpOTop x -> TmDecl
    { decl = DeclOp
      { ident = x.n.v
      , tyAnnot =
        let ty = convType x.ty in
        foldr ntyall_ ty (setToSeq (findVars (setEmpty nameCmp) ty))
      , info = x.info
      }
    , ty = tyunknown_
    , inexpr = cont
    , info = x.info
    }
end

lang ConvertLetImplOTop = ConvertOCamlToMExpr + LetImplOTopAst + OpImplAst + EvalCost + SymCost + FindVars
  sem convTop cont =
  | LetImplOTop x -> TmDecl
    { decl = DeclOpImpl
      { ident = x.n.v
      , implId = negi 1
      , reprScope = negi 1
      , metaLevel = negi 1
      , selfCost = evalCost (symbolizeCost (convExpr x.cost))
      , body = convExpr x.body
      , specType =
        let ty = optionMapOr tyunknown_ convType x.ty in
        foldr ntyall_ ty (setToSeq (findVars (setEmpty nameCmp) ty))
      , delayedReprUnifications = []
      , info = x.info
      }
    , inexpr = cont
    , ty = tyunknown_
    , info = x.info
    }
end

lang ConvertReprOTop = ConvertOCamlToMExpr + ReprOTopAst + ReprDeclAst + VarTypeAst + FindVars + DeclAst
  sem convTop cont =
  | ReprOTop x ->
    let pat = convType x.lhs in
    let repr = convType x.rhs in
    TmDecl
    { decl = DeclRepr
      { ident = x.n.v
      , vars = setToSeq (findVars (findVars (setEmpty nameCmp) pat) repr)
      , pat = pat
      , repr = repr
      , info = x.info
      }
    , ty = tyunknown_
    , inexpr = cont
    , info = x.info
    }
end

lang ConvertUtestOTop = ConvertOCamlToMExpr + UTestOTopAst + UtestDeclAst
  sem convTop cont =
  | UTestOTop x -> TmDecl
    { decl = DeclUtest
      { test = convExpr x.test
      , expected = convExpr x.expected
      , tusing = optionMap convExpr x.tusing
      , tonfail = optionMap convExpr x.onfail
      , info = x.info
      }
    , info = x.info
    , ty = tyunknown_
    , inexpr = cont
    }
end

-- Type bindings --

lang ConvertSimpleOTyBinding = ConvertOCamlToMExpr + SimpleOTyBindingAst
  sem convTyBinding =
  | SimpleOTyBinding x ->
    let paramNames = map (lam x. x.v) x.params in
    let tyDef = lam cont.
      let rhs = optionMapOr (tyvariant_ []) convType x.alias in
      withInfo x.info (bind_ (ntype_ x.n.v paramNames rhs) cont) in
    let conDefs = lam cont.
      let mkConDef = lam constr. lam cont.
        let ty = tyWithInfo x.n.i (ntycon_ x.n.v) in
        let ty = tyapps_ ty (map ntyvar_ paramNames) in
        let ty = tyarrow_ (optionMapOr tyunit_ convType constr.ty) ty in
        let ty = foldr ntyall_ ty paramNames in
        bind_ (ncondef_ constr.n.v ty) cont in
      foldr mkConDef cont x.constructors in
    {tyDef = tyDef, conDefs = conDefs}
end

-- Parameters --

lang ConvertPatOParam = ConvertOCamlToMExpr + PatOParamAst
  sem convParam =
  | PatOParam x -> (convPat x.pat, optionMapOr tyunknown_ convType x.ty)
end

lang ConvertNameOParam = ConvertOCamlToMExpr + NameOParamAst
  sem convParam =
  | NameOParam x -> (withInfoPat x.info (npvar_ x.n.v), tyWithInfo x.info tyunknown_)
end

lang ConvertIgnoreOParam = ConvertOCamlToMExpr + IgnoreOParamAst
  sem convParam =
  | IgnoreOParam x -> (withInfoPat x.info pvarw_, tyunknown_)
end

-- Bindings --

lang ConvertSimpleOBinding = ConvertOCamlToMExpr + SimpleOBindingAst
  sem convBinding =
  | SimpleOBinding x ->
    (x.n.v, map convParam x.params, optionMapOr tyunknown_ convType x.ty, convExpr x.body)
end

-- Exprs --

lang ConvertVarOExpr = ConvertOCamlToMExpr + VarOExprAst
  sem convExpr =
  | VarOExpr x -> withInfo x.info (nvar_ x.n.v)
end

lang ConvertNumOExpr = ConvertOCamlToMExpr + NumOExprAst
  sem convExpr =
  | NumOExpr x -> switch (x.i, x.f)
    case (Some t, _) then withInfo x.info (int_ t.v)
    case (_, Some t) then withInfo x.info (float_ t.v)
    end
end

lang ConvertStringOExpr = ConvertOCamlToMExpr + StringOExprAst + OCamlStringAst
  sem convExpr =
  | StringOExpr x -> withInfo x.info (uconst_ (COString {val = x.v.v}))
end

lang ConvertCharOExpr = ConvertOCamlToMExpr + CharOExprAst
  sem convExpr =
  | CharOExpr x -> withInfo x.info (char_ x.v.v)
end

lang ConvertTrueOExpr = ConvertOCamlToMExpr + TrueOExprAst
  sem convExpr =
  | TrueOExpr x -> withInfo x.info true_
end

lang ConvertFalseOExpr = ConvertOCamlToMExpr + FalseOExprAst
  sem convExpr =
  | FalseOExpr x -> withInfo x.info false_
end

lang ConvertConOExpr = ConvertOCamlToMExpr + ConOExprAst + AppOExprAst
  sem convExpr =
  | ConOExpr x -> withInfo x.info (nconapp_ x.n.v unit_)
  | AppOExpr
    { left = ConOExpr c
    , right = arg
    , info = info
    } -> withInfo info (nconapp_ c.n.v (convExpr arg))
end

lang UnbreakList = ConvertOCamlToMExpr + SemiOExprAst
  sem unbreakList =
  | SemiOExpr x -> cons (convExpr x.left) (unbreakList x.right)
  | tm -> [convExpr tm]
end

lang ConvertListOExpr = ConvertOCamlToMExpr + ListOExprAst + UnbreakList + OCamlListAst
  sem convExpr =
  | ListOExpr x -> withInfo x.info
    (foldr
      (lam elem. lam acc.
        appf2_ (uconst_ (COCons ())) elem acc)
      (uconst_ (CONil ()))
      (optionMapOr [] unbreakList x.unbrokenElems))
end

lang ConvertArrayOExpr = ConvertOCamlToMExpr + ArrayOExprAst
  sem convExpr =
  | ArrayOExpr x -> never
end

lang ConvertUnitOExpr = ConvertOCamlToMExpr + UnitOExprAst
  sem convExpr =
  | UnitOExpr x -> withInfo x.info unit_
end

lang ConvertScaleBaseOExpr = ConvertOCamlToMExpr + ScaleBaseOExprAst + AppOExprAst + VarOExprAst + EvalCost + SymCost + OpVarAst
  sem convExpr =
  | AppOExpr
    { left = AppOExpr
      { left = ScaleBaseOExpr _
      , right = cost
      }
    , right = VarOExpr x
    , info = info
    } -> TmOpVar
      { ident = x.n.v
      , ty = tyunknown_
      , info = info
      , frozen = false
      , scaling = evalCost (symbolizeCost (convExpr cost))
      }
end

lang ConvertIfOExpr = ConvertOCamlToMExpr + IfOExprAst
  sem convExpr =
  | IfOExpr x -> withInfo x.info (if_ (convExpr x.c) (convExpr x.t) (convExpr x.e))
end

lang ConvertLetOExpr = ConvertOCamlToMExpr + LetOExprAst + MkLet
  sem convExpr =
  | LetOExpr x -> mkLet x.info x.rec x.bindings (convExpr x.inexpr)
end

lang ConvertFunOExpr = ConvertOCamlToMExpr + FunOExprAst + MkBody
  sem convExpr =
  | FunOExpr x ->
    mkBody (map (lam x. (convParam x).0) x.params) (convExpr x.body)
end

lang ConvertMatchingOExpr = ConvertOCamlToMExpr + MatchingOExprAst + DeclAst + LetDeclAst
  sem convExpr =
  | MatchingOExpr x ->
    let scrut = nameSym "scrut" in
    let scrutE = withInfo (get_OExpr_info x.scrut) (nvar_ scrut) in
    let scrutLet = nulet_ scrut (convExpr x.scrut) in
    let mkArm = lam arm. lam cont.
      match_ scrutE (convPat arm.pat) (convExpr arm.body) cont
    in
    TmDecl
    { decl = DeclLet
      { ident = scrut
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = convExpr x.scrut
      , info = x.info
      }
    , inexpr = foldr mkArm never_ x.arms
    , ty = tyunknown_
    , info = x.info
    }
end

lang ConvertAddiOExpr = ConvertOCamlToMExpr + AddiOExprAst + ArithIntAst
  sem convExpr =
  | AddiOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CAddi ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertSubiOExpr = ConvertOCamlToMExpr + SubiOExprAst + ArithIntAst
  sem convExpr =
  | SubiOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CSubi ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertMuliOExpr = ConvertOCamlToMExpr + MuliOExprAst + ArithIntAst
  sem convExpr =
  | MuliOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CMuli ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertDiviOExpr = ConvertOCamlToMExpr + DiviOExprAst + ArithIntAst
  sem convExpr =
  | DiviOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CDivi ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertAddfOExpr = ConvertOCamlToMExpr + AddfOExprAst + ArithFloatAst
  sem convExpr =
  | AddfOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CAddf ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertSubfOExpr = ConvertOCamlToMExpr + SubfOExprAst + ArithFloatAst
  sem convExpr =
  | SubfOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CSubf ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertMulfOExpr = ConvertOCamlToMExpr + MulfOExprAst + ArithFloatAst
  sem convExpr =
  | MulfOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CMulf ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertDivfOExpr = ConvertOCamlToMExpr + DivfOExprAst + ArithFloatAst
  sem convExpr =
  | DivfOExpr x -> withInfo x.info (appf2_
    (withInfo x.op (uconst_ (CDivf ())))
    (convExpr x.left)
    (convExpr x.right))
end

lang ConvertEqOExpr = ConvertOCamlToMExpr + EqOExprAst + OCamlCmpAst
  sem convExpr =
  | EqOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpEq ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertNeqOExpr = ConvertOCamlToMExpr + NeqOExprAst + OCamlCmpAst
  sem convExpr =
  | NeqOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpNeq ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertLtOExpr = ConvertOCamlToMExpr + LtOExprAst + OCamlCmpAst
  sem convExpr =
  | LtOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpLt ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertGtOExpr = ConvertOCamlToMExpr + GtOExprAst + OCamlCmpAst
  sem convExpr =
  | GtOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpGt ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertLeqOExpr = ConvertOCamlToMExpr + LeqOExprAst + OCamlCmpAst
  sem convExpr =
  | LeqOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpLeq ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertGeqOExpr = ConvertOCamlToMExpr + GeqOExprAst + OCamlCmpAst
  sem convExpr =
  | GeqOExpr x -> withInfo x.info (appf2_
    (mkOp x.info (OpGeq ()))
    (convExpr x.left)
    (convExpr x.right)
  )
end

lang ConvertTupOExpr = ConvertOCamlToMExpr + TupOExprAst
  sem convExpr =
  | tm & TupOExpr x -> withInfo x.info (utuple_ (tupList tm))

  sem tupList =
  | TupOExpr x -> snoc (tupList x.left) (convExpr x.right)
  | tm -> [convExpr tm]
end

lang ConvertAppOExpr = ConvertOCamlToMExpr + AppOExprAst
  sem convExpr =
  | AppOExpr x -> withInfo x.info (app_ (convExpr x.left) (convExpr x.right))
end

lang ConvertConsOExpr = ConvertOCamlToMExpr + ConsOExprAst + OCamlListAst
  sem convExpr =
  | ConsOExpr x -> appf2_ (uconst_ (COCons ())) (convExpr x.left) (convExpr x.right)
end

lang ConvertSemiOExpr = ConvertOCamlToMExpr + SemiOExprAst
  sem convExpr =
  | SemiOExpr x -> withInfo x.info (semi_ (convExpr x.left) (convExpr x.right))
end

lang ConvertOrOExpr = ConvertOCamlToMExpr + OrOExprAst
  sem convExpr =
  | OrOExpr x -> withInfo x.info (if_ (convExpr x.left) true_ (convExpr x.right))
end

lang ConvertAndOExpr = ConvertOCamlToMExpr + AndOExprAst
  sem convExpr =
  | AndOExpr x -> withInfo x.info (if_ (convExpr x.left) (convExpr x.right) false_)
end

lang ConvertAccessOExpr = ConvertOCamlToMExpr + AccessOExprAst + ConOExprAst + OpaqueOCamlAst
  sem convExpr =
  | tm & AccessOExpr (x & {module = Some _, field = None _, idx = None _}) ->
    let components = moduleChain tm in
    TmOpaqueOCaml
    { info = x.info
    , content = strJoin "." (map nameGetStr components)
    , ty = tyunknown_
    }
  | AccessOExpr (x & {field = Some f, module = None _, idx = None _}) ->
    let components = moduleChain x.left in
    TmOpaqueOCaml
    { info = x.info
    , content = strJoin "." (snoc (map nameGetStr components) f.v)
    , ty = tyunknown_
    }
  | AccessOExpr (x & {idx = Some idx, field = None _, module = None _}) ->
    never "array indexing"

  sem moduleChain =
  | ConOExpr x -> [x.n.v]
  | AccessOExpr (x & {module = Some n}) -> snoc (moduleChain x.left) n.v
  | tm -> errorSingle [get_OExpr_info tm] "Expected a module name here"
end

lang ConvertHoleOExpr = ConvertOCamlToMExpr + HoleOExprAst + HoleAst
  sem convExpr =
  | HoleOExpr x ->
    let errorFieldMultipleDefs = lam i. lam s.
      errorSingle [i] (concat "Multiple definitions of " s)
    in

    let expectMExprInt = lam i. lam e.
      match convExpr e with TmConst {val = CInt v} then v.val
      else errorSingle [i] "Expected an integer"
    in

    let parseField = lam acc. lam field.
      switch nameGetStr field.name.v
      case "default" then
        if optionIsNone acc.default then {acc with default = Some field}
        else errorFieldMultipleDefs field.name.i "default"
      case "min" then
        if optionIsNone acc.min then {acc with min = Some field}
        else errorFieldMultipleDefs field.name.i "min"
      case "max" then
        if optionIsNone acc.max then {acc with max = Some field}
        else errorFieldMultipleDefs field.name.i "max"
      case "depth" then
        if optionIsNone acc.depth then {acc with depth = Some field}
        else errorFieldMultipleDefs field.name.i "depth"
      case _ then errorSingle [field.name.i] "Unknown field"
      end
    in

    let fields = foldl parseField
      {default = None (), min = None (), max = None (), depth = None ()}
      x.fields
    in

    let inner =
      switch (fields.min, fields.max)
      case (None (), None ()) then BoolHole {}
      case (Some min, Some max) then
        HIntRange
        { min = expectMExprInt min.name.i min.val
        , max = expectMExprInt max.name.i max.val
        }
      case _ then errorSingle [x.info] "Did you forget to specify min or max?"
      end
    in

    let default =
      optionMapOrElse
        (lam. errorSingle [x.info] "Missing default")
        (lam d. convExpr d.val)
        fields.default
    in

    TmHole
    { inner = inner
    , default = default
    , info = x.info
    , ty = tyunknown_
    , depth = optionMapOr 0 (lam d. expectMExprInt x.info d.val) fields.depth
    }
end

-- Patterns --

lang ConvertWildOPat = ConvertOCamlToMExpr + WildOPatAst
  sem convPat =
  | WildOPat x -> withInfoPat x.info pvarw_
end

lang ConvertBindOPat = ConvertOCamlToMExpr + BindOPatAst
  sem convPat =
  | BindOPat x -> withInfoPat x.info (npvar_ x.n.v)
end

lang ConvertNumOPat = ConvertOCamlToMExpr + NumOPatAst
  sem convPat =
  | NumOPat x -> withInfoPat x.info (pint_ x.i.v)
end

lang ConvertStringOPat = ConvertOCamlToMExpr + StringOPatAst + OCamlStringAst
  sem convPat =
  | StringOPat x -> PatOString {info = x.info, val = x.v.v, ty = tyunknown_}
end

lang ConvertCharOPat = ConvertOCamlToMExpr + CharOPatAst
  sem convPat =
  | CharOPat x -> withInfoPat x.info (pchar_ x.v.v)
end

lang ConvertTrueOPat = ConvertOCamlToMExpr + TrueOPatAst
  sem convPat =
  | TrueOPat x -> withInfoPat x.info ptrue_
end

lang ConvertFalseOPat = ConvertOCamlToMExpr + FalseOPatAst
  sem convPat =
  | FalseOPat x -> withInfoPat x.info ptrue_
end

lang ConvertConOPat = ConvertOCamlToMExpr + ConOPatAst + OpaqueOCamlAst
  sem convPat =
  | ConOPat x ->
    match x.parts with [n] then
      withInfoPat x.info (npcon_ n.v pvarw_)
    else PatOpaqueOCamlCon
      { info = x.info
      , content = strJoin "." (map (lam x. nameGetStr x.v) x.parts)
      , arg = None ()
      , ty = tyunknown_
      }
end

lang ConvertListOPat = ConvertOCamlToMExpr + ListOPatAst + OCamlListAst
  sem convPat =
  | ListOPat x -> withInfoPat x.info (foldr
    (lam elem. lam acc. PatOCons
      { head = convPat elem
      , tail = acc
      , info = mergeInfo (get_OPat_info elem) (infoPat acc)
      , ty = tyunknown_
      })
    (PatONil {info = x.info, ty = tyunknown_})
    x.elems)
end

lang ConvertArrayOPat = ConvertOCamlToMExpr + ArrayOPatAst
  sem convPat =
  | ArrayOPat x -> never
end

lang ConvertUnitOPat = ConvertOCamlToMExpr + UnitOPatAst
  sem convPat =
  | UnitOPat x -> withInfoPat x.info punit_
end

lang ConvertAppOPat = ConvertOCamlToMExpr + AppOPatAst + ConOPatAst + OpaqueOCamlAst
  sem convPat =
  | AppOPat (x & {left = ConOPat c}) ->
    match c.parts with [n] then
      withInfoPat x.info (npcon_ n.v (convPat x.right))
    else PatOpaqueOCamlCon
      { info = x.info
      , content = strJoin "." (map (lam x. nameGetStr x.v) c.parts)
      , arg = Some (convPat x.right)
      , ty = tyunknown_
      }
end

lang ConvertOrOPat = ConvertOCamlToMExpr + OrOPatAst
  sem convPat =
  | OrOPat x -> withInfoPat x.info (por_ (convPat x.left) (convPat x.right))
end

lang ConvertTupOPat = ConvertOCamlToMExpr + TupOPatAst
  sem convPat =
  | pat & TupOPat x -> withInfoPat x.info (ptuple_ (patTupList pat))

  sem patTupList =
  | TupOPat x -> snoc (patTupList x.left) (convPat x.right)
  | pat -> [convPat pat]
end

lang ConvertConsOPat = ConvertOCamlToMExpr + ConsOPatAst + OCamlListAst
  sem convPat =
  | ConsOPat x -> PatOCons
    { info = x.info
    , ty = tyunknown_
    , head = convPat x.left
    , tail = convPat x.right
    }
end

lang ConvertAsOPat = ConvertOCamlToMExpr + AsOPatAst
  sem convPat =
  | AsOPat x -> withInfoPat x.info (pand_ (convPat x.left) (withInfoPat x.n.i (npvar_ x.n.v)))
end

-- Types --

lang ConvertVarOType = ConvertOCamlToMExpr + VarOTypeAst
  sem convType =
  | VarOType x -> tyWithInfo x.info (ntyvar_ x.n.v)
end

lang ConvertConOType = ConvertOCamlToMExpr + ConOTypeAst
  sem convType =
  | ConOType x -> tyWithInfo x.info (ntycon_ x.n.v)
end

lang ConvertWildOType = ConvertOCamlToMExpr + WildOTypeAst + TyWildAst
  sem convType =
  | WildOType x -> TyWild {info = x.info}
end

lang ConvertSubstOType = ConvertOCamlToMExpr + SubstOTypeAst + AppOTypeAst + ReprSubstAst + TyWildAst + ReprTypeAst
  sem convType =
  | SubstOType x -> TySubst
    { info = x.info
    , arg =
      let mkRepr = lam. TyRepr
        { info = x.info
        , arg = TyWild {info = x.info}
        , repr = ref (UninitRepr ())
        } in
      optionMapOrElse mkRepr convType x.ty
    , subst = x.n.v
    }
end

lang ConvertAppOType = ConvertOCamlToMExpr + AppOTypeAst + CommaOTypeAst + OpaqueOCamlAst
  sem convType =
  | AppOType x ->
    let tyCon = if null x.modules
      then tyWithInfo x.n.i (ntycon_ x.n.v)
      else TyOpaqueOCaml
        { info = x.info
        , content = strJoin "." (map (lam x. nameGetStr x.v) x.modules)
        } in
    foldl (lam acc. lam ty. tyWithInfo x.info (tyapp_ acc ty)) tyCon (tyArgList x.left)

  sem tyArgList =
  | CommaOType x -> snoc (tyArgList x.left) (convType x.right)
  | ty -> [convType ty]
end

lang ConvertTupOType = ConvertOCamlToMExpr + TupOTypeAst
  sem convType =
  | ty & TupOType x -> tyWithInfo x.info (tytuple_ (tyTupList ty))

  sem tyTupList =
  | TupOType x -> snoc (tyTupList x.left) (convType x.right)
  | ty -> [convType ty]
end

lang ConvertArrowOType = ConvertOCamlToMExpr + ArrowOTypeAst
  sem convType =
  | ArrowOType x -> tyarrow_ (convType x.left) (convType x.right)
end

-- Compose it all --

lang ComposedConvertOCamlToMExpr
  = ConvertAccessOExpr
  + ConvertAddfOExpr
  + ConvertAddiOExpr
  + ConvertAndOExpr
  + ConvertAppOExpr
  + ConvertHoleOExpr
  + ConvertAppOPat
  + ConvertAppOType
  + ConvertArrayOExpr
  + ConvertArrayOPat
  + ConvertArrowOType
  + ConvertAsOPat
  + ConvertBindOPat
  + ConvertCharOExpr
  + ConvertCharOPat
  + ConvertConOExpr
  + ConvertConOPat
  + ConvertConOType
  + ConvertConsOExpr
  + ConvertConsOPat
  + ConvertDivfOExpr
  + ConvertEqOExpr
  + ConvertNeqOExpr
  + ConvertLtOExpr
  + ConvertGtOExpr
  + ConvertLeqOExpr
  + ConvertGeqOExpr
  + ConvertDiviOExpr
  + ConvertFalseOExpr
  + ConvertFalseOPat
  + ConvertFunOExpr
  + ConvertIfOExpr
  + ConvertIgnoreOParam
  + ConvertLetImplOTop
  + ConvertLetOExpr
  + ConvertLetOpOTop
  + ConvertLetOTop
  + ConvertListOExpr
  + ConvertListOPat
  + ConvertMatchingOExpr
  + ConvertMulfOExpr
  + ConvertMuliOExpr
  + ConvertNameOParam
  + ConvertNumOExpr
  + ConvertNumOPat
  + ConvertOrOExpr
  + ConvertOrOPat
  + ConvertPatOParam
  + ConvertReprOTop
  + ConvertScaleBaseOExpr
  + ConvertSemiOExpr
  + ConvertSimpleOBinding
  + ConvertSimpleOTyBinding
  + ConvertStringOExpr
  + ConvertStringOPat
  + ConvertSubfOExpr
  + ConvertSubiOExpr
  + ConvertSubstOType
  + ConvertTrueOExpr
  + ConvertTrueOPat
  + ConvertTupOExpr
  + ConvertTupOPat
  + ConvertTupOType
  + ConvertTypeOTop
  + ConvertUnitOExpr
  + ConvertUnitOPat
  + ConvertUtestOTop
  + ConvertVarOExpr
  + ConvertVarOExpr
  + ConvertVarOType
  + ConvertWildOPat
  + ConvertWildOType
end
