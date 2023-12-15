include "mexpr/eval.mc"
include "mexpr/symbolize.mc"

-- NOTE(vipa, 2023-11-03): I'll probably end up regretting this piece
-- of global state at some later point
let costEvalEnv /- : Ref EvalEnv (defined in lang frag) -/ = use Eval in ref (evalEnvEmpty ())
let costSymEnv : Ref SymEnv = ref symEnvEmpty

let costSetVar
  : Name -> use Ast in Expr -> ()
  = lam n. lam c.
    let n = if nameHasSym n then n else nameSetNewSym n in
    modref costSymEnv (
      let old = deref costSymEnv in
      { old with varEnv = mapInsert (nameGetStr n) n old.varEnv }
    );
    use Eval in modref costEvalEnv (evalEnvInsert n c (deref costEvalEnv))

lang SymCost = Sym
  sem symbolizeCost : Expr -> Expr
  sem symbolizeCost = | tm ->
    symbolizeExpr (deref costSymEnv) tm
end

lang EvalCost = Eval + FloatAst + PrettyPrint
  sem evalCost : Expr -> Float
  sem evalCost = | tm ->
    switch eval {env = deref costEvalEnv} tm
    case TmConst {val = CFloat {val = val}} then
      val
    case res then
      errorSingle [infoTm tm] (concat "* A cost has to evaluate to a float, this cost evaluated to:\n* " (expr2str res))
    end
end

lang LogfBuiltin = Ast + Eval + PrettyPrint + ConstArity + FloatAst + ConstDelta + ConstPrettyPrint
  syn Const =
  | CLogf ()

  sem constArity =
  | CLogf _ -> 1

  sem delta info =
  | (CLogf _, [TmConst (t & {val = CFloat f})]) ->
    TmConst {t with val = CFloat {val = log f.val}}

  sem getConstStringCode indent =
  | CLogf _ -> "log"
end
