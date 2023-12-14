include "mexpr/const-arity.mc"

include "ast.mc"

lang OCamlStringConstArity = ConstArity + OCamlStringAst
  sem constArity =
  | COString _ -> 0
end

lang OCamlListConstArity = PrettyPrint + OCamlListAst
  sem constArity =
  | CONil _ -> 0
  | COCons _ -> 2
end

lang OCamlExtrasConstArity = OCamlStringConstArity + OCamlListConstArity
end
