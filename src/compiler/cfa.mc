include "mexpr/cfa.mc"

include "ast.mc"

lang OCamlStringCFA = ConstCFA + OCamlStringAst
  sem generateConstraintsConst graph info ident =
  | COString _ -> graph
end

lang OCamlOpaqueCFA = CFA + OpaqueOCamlAst
  sem generateConstraints graph =
  | TmDecl {decl = DeclLet {ident = ident, body = TmOpaqueOCaml _}} -> graph

end

lang OCamlListCFA = ConstCFA + OCamlListAst
  sem generateConstraintsConst graph info ident =
  | CONil _ -> graph
  | COCons _ -> graph
end

lang OCamlExtrasCFA = OCamlStringCFA + OCamlOpaqueCFA + OCamlListCFA
end
