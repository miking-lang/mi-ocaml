-- stdlib
include "arg.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/phase-stats.mc"
include "mexpr/reptypes.mc"
include "ocaml/mcore.mc"

-- local
include "syntax.mc"
include "convert.mc"
include "pprint.mc"
include "prelude.mc"
include "type-check.mc"
include "shallow-patterns.mc"
include "generate.mc"

lang MCoreCompile =
  OCamlAst +
  ComposedConvertOCamlToMExpr +
  MExprCmp +
  MExprSym + MExprTypeCheck +
  MExprPrettyPrintWithReprs +
  MExprLowerNestedPatterns +
  MCoreCompileLang + PhaseStats +
  RepTypesFragments +
  DumpRepTypesProblem +
  PrintMostFrequentRepr +
  PprintTyAnnot + HtmlAnnotator +
  MExprEval + MetaVarTypePrettyPrint +

  OCamlPrelude + LogfBuiltin +
  OCamlExtrasPprint + OCamlExtrasTypeCheck + ShallowOCamlExtras + OCamlExtrasGenerate
end

lang RepAnalysis = MExprRepTypesAnalysis + MExprCmp + MExprPrettyPrint + OCamlExtrasTypeCheck + OCamlExtrasPprint
end

lang MExprRepTypesComposedSolver
  = RepTypesComposedSolver
  + MExprAst
  + MExprPrettyPrintWithReprs
  + RepTypesAst
  + OCamlExtrasPprint
  + MetaVarTypePrettyPrint
  + MetaVarTypeGeneralize
  + VarTypeGeneralize
  + AllTypeGeneralize
  + MExprUnify
  + ReprTypeUnify
  + TyWildUnify
  + OCamlExtrasTypeCheck

  sem getTypeStringCode indent env =
  | ty -> errorSingle [infoTy ty] "Missing getTypeStringCode"
end

mexpr

use MCoreCompile in

let options =
  { olibs = []
  , clibs = []
  , debugMExpr = None ()
  , debugSolver = false
  , destinationFile = None ()
  , jsonPath = None ()
  } in

let argConfig =
  [ ( [("--olib", "", "")]
    , "Add a dependency on an Opam package."
    , lam p. { p.options with olibs = snoc p.options.olibs (argToString p) }
    )
  , ( [("--clib", "", "")]
    , "Tell dune/ocamlopt to link these c-libraries."
    , lam p. { p.options with olibs = snoc p.options.olibs (argToString p) }
    )
  , ( [("--output", " ", "<path>")]
    , "Place the final executable here."
    , lam p. { p.options with destinationFile = Some (argToString p) }
    )
  , ( [("--debug-mexpr", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after conversion to MExpr."
    , lam p. { p.options with debugMExpr = Some (argToString p) }
    )
  , ( [("--define", " ", "<binding>")]
    , "Define a constant to be used in impl costs."
    , lam p. match strSplit "=" (argToString p) with [name, value] then
        costSetVar (nameNoSym name) (float_ (string2float value));
        p.options
      else error "A binding must have the form \"<name>=<value>\"."
    )
  , ( [("--debug-solver", "", "")]
    , "Print debug information about the state of the solver after each letimpl."
    , lam p. { p.options with debugSolver = true }
    )
  , ( [("--constraints-json", " ", "<path>")]
    , "Output the connections between representations and operations in JSON format."
    , lam p. { p.options with jsonPath = Some (argToString p) }
    )
  ] in

match
  let res = argParse options argConfig in
  match res with ParseOK res then
    match res.strings with [mlFile] then
      (res.options, mlFile)
    else error (concat "Need exactly one positional argument (the .ml file to compile), got: " (strJoin ", " res.strings))
  else argPrintError res
with (options, mlFile) in

costSetVar (nameNoSym "log") (uconst_ (CLogf ()));

let parseOCamlExn : String -> String -> OFile = parseOCamlExn in
match parseOCamlExn mlFile (readFile mlFile) with TopsOFile {tops = ast} in
let ast = ocamlToMExpr ast in
let ast = wrapInPrelude ast in

(match options.debugMExpr with Some path then
   writeFile path (pprintAst ast)
 else ());

let ast = symbolize ast in
let ast = typeCheckLeaveMeta ast in
let ast = use RepAnalysis in typeCheckLeaveMeta ast in

(match options.jsonPath with Some jsonPath then
   dumpRepTypesProblem jsonPath ast
 else ());

let ast = use MExprRepTypesComposedSolver in reprSolve options.debugSolver ast in
let ast = removeMetaVarExpr ast in
let ast = lowerAll ast in

match options.destinationFile with Some destinationFile in

compileMCore ast
  { debugTypeAnnot = lam. ()
  , debugGenerate = lam. ()
  , exitBefore = lam. ()
  , postprocessOcamlTops = lam x. x
  , compileOcaml = lam libs. lam clibs. lam srcStr.
    let config =
      { optimize = true
      , libraries = concat libs options.olibs
      , cLibraries = concat clibs options.clibs
      } in
    let res = ocamlCompileWithConfig config srcStr in
    sysMoveFile res.binaryPath destinationFile;
    sysChmodWriteAccessFile destinationFile;
    res.cleanup ();
    ()
  };

()
