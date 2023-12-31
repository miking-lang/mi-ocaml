include "mexpr/anf.mc"

include "ast.mc"

lang OCamlOpaqueANF = ANF + OpaqueOCamlAst
  sem normalize (k : Expr -> Expr) =
  | TmOpaqueOCaml t -> k (TmOpaqueOCaml t)
end

lang OCamlExtrasANF = OCamlOpaqueANF
end
