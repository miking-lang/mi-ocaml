-- stdlib
include "arg.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/op-overload.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/phase-stats.mc"
include "mexpr/reptypes.mc"
include "mexpr/utest-generate.mc"
include "ocaml/mcore.mc"
include "tuning/ast.mc"
include "tuning/context-expansion.mc"
include "tuning/dependency-analysis.mc"
include "tuning/instrumentation.mc"
include "tuning/tune.mc"
include "tuning/tune-file.mc"

-- local
include "syntax.mc"
include "convert.mc"
include "pprint.mc"
include "prelude.mc"
include "type-check.mc"
include "shallow-patterns.mc"
include "generate.mc"
include "cmp.mc"
include "anf.mc"
include "cfa.mc"
include "const-arity.mc"

lang MCoreCompile
  = OCamlAst
  + ComposedConvertOCamlToMExpr
  + DumpRepTypesProblem
  + HoleAst
  + HtmlAnnotator
  + LogfBuiltin
  + MCoreCompileLang
  + MetaVarTypePrettyPrint
  + MExprCmp
  + MExprEval
  + MExprLowerNestedPatterns
  + MExprPrettyPrint
  + MExprSym
  + MExprTypeCheck
  + MExprUtestGenerate
  + OCamlExtrasCmp
  + OCamlExtrasGenerate
  + OCamlExtrasPprint
  + OCamlExtrasTypeCheck
  + OCamlPrelude
  + OverloadedOpDesugar
  + OverloadedOpTypeCheck
  + PhaseStats
  + PprintTyAnnot
  + PrintMostFrequentRepr
  + RepTypesCmp
  + RepTypesFragments
  + RepTypesPrettyPrint
  + RepTypesSym
  + RepTypesTypeCheck
  + ShallowOCamlExtras
end

lang RepAnalysis
  = MExprRepTypesAnalysis
  + MExprCmp
  + RepTypesCmp
  + MExprPrettyPrint
  + RepTypesPrettyPrint
  + OCamlExtrasTypeCheck
  + OCamlExtrasPprint
end

lang MExprRepTypesSolverBase
  = AllTypeGeneralize
  + MetaVarTypeGeneralize
  + MetaVarTypePrettyPrint
  + MExprAst
  + MExprCmp
  + MExprPrettyPrint
  + MExprUnify
  + OCamlExtrasCmp
  + OCamlExtrasPprint
  + OCamlExtrasTypeCheck
  + ReprTypeUnify
  + RepTypesAst
  + RepTypesCmp
  + RepTypesPrettyPrint
  + RepTypesSolveAndReconstruct
  + TyWildUnify
  + VarTypeGeneralize

  sem getTypeStringCode indent env =
  | ty -> errorSingle [infoTy ty] "Missing getTypeStringCode"
end

lang MExprTuning
  = MExprHoles
  + MExprHoleCFA
  + NestedMeasuringPoints
  + DependencyAnalysis
  + Instrumentation
  + MExprTune
  + MExprTypeCheck
  + OCamlExtrasTypeCheck
  + OCamlExtrasPprint
  + OCamlExtrasANF
  + OCamlExtrasCFA
  + OCamlExtrasConstArity
  + RepTypesPrettyPrint
end

lang MExprTuneANFAll
  = HoleAst
  + MExprANFAll
  + OCamlExtrasANF
end

lang ComposedSATIshSolver
  = MExprRepTypesSolverBase
  + SolTreeSoloSolver
  + SATishSolver
end

lang ComposedLazyTopDownSolver
  = MExprRepTypesSolverBase
  + LazyTopDownSolver
end

lang ComposedMemoedTopDownSolver
  = MExprRepTypesSolverBase
  + MemoedTopDownSolver
end

mexpr

use MCoreCompile in

type SolverOption in
con SATishSolver : () -> SolverOption in
con LazyTopDownSolver : () -> SolverOption in
con MemoedTopDownSolver : () -> SolverOption in

let options =
  { olibs = []
  , clibs = []

  , reprSolver = LazyTopDownSolver ()
  , useRepr = true
  , useTuning = true
  , debugMExpr = None ()
  , debugTypeCheck = None ()
  , debugDesugar = None ()
  , debugRepr = None ()
  , debugAnalysis = None ()
  , debugSolverState = false
  , debugFinalSolution = false
  , debugSolveProcess = false
  , debugSolveTiming = false
  , destinationFile = None ()
  , generateTests = false
  , jsonPath = None ()
  , inputTunedValues = None ()
  , outputTunedValues = None ()
  , tuneOptions = None ()
  } in
let argConfig =
  [ ( [("--olib", " ", "<package>")]
    , "Add a dependency on an Opam package."
    , lam p. { p.options with olibs = snoc p.options.olibs (argToString p) }
    )
  , ( [("--clib", " ", "<c-library>")]
    , "Tell dune/ocamlopt to link these c-libraries."
    , lam p. { p.options with clibs = snoc p.options.clibs (argToString p) }
    )
  , ( [("--output", " ", "<path>")]
    , "Place the final executable here."
    , lam p. { p.options with destinationFile = Some (argToString p) }
    )
  , ( [("--test", "", "")]
    , "Generate utest code."
    , lam p. { p.options with generateTests = true }
    )
  , ( [("--debug-mexpr", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after conversion to MExpr."
    , lam p. { p.options with debugMExpr = Some (argToString p) }
    )
  , ( [("--debug-type-check", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after type checking."
    , lam p. { p.options with debugTypeCheck = Some (argToString p) }
    )
  , ( [("--debug-desugar", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after desugaring."
    , lam p. { p.options with debugDesugar = Some (argToString p) }
    )

  -- Reptypes related options
  , ( [("--no-repr", "", "")]
    , "Turn off the repr-passes (i.e., programs that contain repr will fail to compile, possibly loudly)."
    , lam p. { p.options with useRepr = false }
    )
  , ( [("--repr-solver", " ", "<solver>")]
    , "Pick the solver to use for picking letimpls"
    , lam p.
      let mapping = mapFromSeq cmpString
        [ ("sat", SATishSolver ())
        , ("lazy-greed", LazyTopDownSolver ())
        , ("top-down", MemoedTopDownSolver ())
        ] in
      let reprSolver = argToString p in
      match mapLookup reprSolver mapping with Some reprSolver
      then { p.options with reprSolver = reprSolver }
      else
        printLn (concat "Unknown repr-solver: " reprSolver);
        printLn (join
          [ "Expected one of: ", strJoin ", " (mapKeys mapping)
          ]);
        exit 1
    )
  , ( [("--debug-repr", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after repr solving."
    , lam p. { p.options with debugRepr = Some (argToString p) }
    )
  , ( [("--debug-analysis", " ", "<path>")]
    , "Output an interactive (html) pprinted version of the AST just after repr analysis."
    , lam p. { p.options with debugAnalysis = Some (argToString p) }
    )
  , ( [("--define", " ", "<binding>")]
    , "Define a constant to be used in impl costs."
    , lam p. match strSplit "=" (argToString p) with [name, value] then
        costSetVar (nameNoSym name) (float_ (string2float value));
        p.options
      else error "A binding must have the form \"<name>=<value>\"."
    )
  , ( [("--debug-solver-state", "", "")]
    , "Print debug information about the state of the solver after each op-use."
    , lam p. { p.options with debugSolverState = true }
    )
  , ( [("--debug-solve-timing", "", "")]
    , "Print debug information about the state of the solver after each op-use."
    , lam p. { p.options with debugSolveTiming = true }
    )
  , ( [("--debug-final-solution", "", "")]
    , "Print debug information about the final solution."
    , lam p. { p.options with debugFinalSolution = true }
    )
  , ( [("--debug-solve-process", "", "")]
    , "Print debug information about the solving process."
    , lam p. { p.options with debugSolveProcess = true }
    )
  , ( [("--constraints-json", " ", "<path>")]
    , "Output the connections between representations and operations in JSON format."
    , lam p. { p.options with jsonPath = Some (argToString p) }
    )

  -- Tuning related options
  , ( [("--no-tuning", "", "")]
    , "Turn off the tuning-passes (i.e., programs that contain holes will fail to compile, possibly loudly)."
    , lam p. { p.options with useTuning = false }
    )
  , ( [("--with-tuned-values", " ", "<path>")]
    , "Compile with tuned values from this file (no tuning)."
    , lam p. { p.options with inputTunedValues = Some (argToString p) }
    )
  , ( [("--tune-options", " ", "<path>")]
    , "If option --tune is provided, read tune options from this toml file."
    , lam p. { p.options with tuneOptions = Some (argToString p) }
    )
  , ( [("--tune", " ", "<path>")]
    , "Perform tuning and write tuned values to this file."
    , lam p. { p.options with outputTunedValues = Some (argToString p) }
    )
  ] in

let compile : [String] -> [String] -> Expr -> String -> () =
  lam olibs. lam clibs. lam ast. lam destinationFile.
    compileMCore ast
      { debugTypeAnnot = lam. ()
      , debugGenerate = lam. ()
      , exitBefore = lam. ()
      , postprocessOcamlTops = lam x. x
      , compileOcaml = lam ol. lam cl. lam srcStr.
        let config =
          { optimize = true
          , libraries = concat ol olibs
          , cLibraries = concat cl clibs
          } in
        let res = ocamlCompileWithConfig config srcStr in
        sysMoveFile res.binaryPath destinationFile;
        sysChmodWriteAccessFile destinationFile;
        res.cleanup ();
        ()
      }
in

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

(match options.debugTypeCheck with Some path then
   writeFile path (pprintAst ast)
 else ());

let ast = desugarExpr ast in

(match options.debugDesugar with Some path then
   writeFile path (pprintAst ast)
 else ());

let ast = generateUtest options.generateTests ast in

let ast =
  if options.useRepr then
    let ast = use RepAnalysis in typeCheckLeaveMeta ast in

    (match options.debugAnalysis with Some path then
       writeFile path (pprintAst ast)
     else ());
    (match options.jsonPath with Some jsonPath then
       dumpRepTypesProblem jsonPath ast
     else ());

    let reprOptions =
      { debugBranchState = options.debugSolverState
      , debugFinalSolution = options.debugFinalSolution
      , debugSolveProcess = options.debugSolveProcess
      , debugSolveTiming = options.debugSolveTiming
      } in
    let ast = switch options.reprSolver
      case SATishSolver _ then use ComposedSATIshSolver in reprSolve reprOptions ast
      case LazyTopDownSolver _ then use ComposedLazyTopDownSolver in reprSolve reprOptions ast
      case MemoedTopDownSolver _ then use ComposedMemoedTopDownSolver in reprSolve reprOptions ast
      end in

    (match options.debugRepr with Some path then
       writeFile path (pprintAst ast)
     else ());
    ast
  else ast
in

let ast =
  if options.useTuning then
    match options.inputTunedValues with Some path then
      use MExprTuning in
      let table = tuneFileReadTable path in
      let ast = normalizeTerm ast in
      match colorCallGraph [] ast with (env, ast) in
      insert env table ast

    else match options.outputTunedValues with Some path then
      use MExprTuning in
      let tuneOptions: TuneOptions = tuneOptionsFromToml
        tuneOptionsDefault (optionMapOr "" readFile options.tuneOptions) in

      let ast = normalizeTerm ast in
      match colorCallGraph [] ast with (env, cAst) in

      match
        if tuneOptions.dependencyAnalysis then
          let ast = use MExprTuneANFAll in normalizeTerm cAst in
          let cfaRes = holeCfa (graphDataInit env) ast in
          let cfaRes = analyzeNested env cfaRes ast in
          (analyzeDependency env cfaRes ast, ast)
        else assumeFullDependency env cAst
      with (dep, ast) in

      match instrument env dep ast with (instRes, ast) in
      match contextExpand env ast with (r, ast) in

      let ast = stripTuneAnnotations ast in
      let ast = typeCheckLeaveMeta ast in
      let ast = removeMetaVarExpr ast in
      let ast = lowerAll ast in

      let tuneBinary = sysJoinPath r.tempDir "tune" in
      compile options.olibs options.clibs ast tuneBinary;

      let result = tune tuneBinary tuneOptions env dep instRes r ast in
      tuneFileDumpTable path env result true;

      r.cleanup();
      instRes.cleanup();

      insert env result cAst

    else
      let ast = stripTuneAnnotations ast in
      default ast

  else ast
in

let ast = removeMetaVarExpr ast in
let ast = lowerAll ast in

match options.destinationFile with Some destinationFile in

compile options.olibs options.clibs ast destinationFile
