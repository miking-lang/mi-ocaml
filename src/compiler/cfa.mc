include "mexpr/cfa.mc"

include "ast.mc"

lang OCamlStringCFA = CFA + ConstCFA + OCamlStringAst
  sem generateConstraintsConst graph info ident =
  | COString _ -> graph
end

lang OCamlOpaqueCFA = CFA + OpaqueOCamlAst
  sem generateConstraints graph =
  | TmLet { ident = ident, body = TmOpaqueOCaml _, info = info } -> graph

end

lang OCamlListCFA = PrettyPrint + OCamlListAst
  sem generateConstraintsConst graph info ident =
  | CONil _ -> graph
  | COCons _ -> graph
end

lang OCamlExtrasCFA = OCamlStringCFA + OCamlOpaqueCFA + OCamlListCFA
end
