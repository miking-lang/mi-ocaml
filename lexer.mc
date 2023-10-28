include "parser/lexer.mc"

lang SemiSemiTokenParser = TokenParser
  syn Token =
  | SemiSemiTok {info : Info}
  syn TokenRepr =
  | SemiSemiRepr ()

  sem parseToken (pos : Pos) =
  | ";;" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = SemiSemiTok {info = info}, lit = ";;", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq (tokRepr : TokenRepr) =
  | SemiSemiTok _ -> match tokRepr with SemiSemiRepr _ then true else false

  sem tokInfo =
  | SemiSemiTok {info = info} -> info

  sem tokReprToStr =
  | SemiSemiRepr _ -> "<SemiSemi>"

  sem tokToStr =
  | SemiSemiTok _ -> "<SemiSemi>"

  sem tokToRepr =
  | SemiSemiTok _ -> SemiSemiRepr ()
end

-- Eat multiline comment of the form /-  -/
lang OCamlMultilineCommentParser = WSACParser
  sem eatWSAC (p : Pos) =
  | "(*" ++ xs ->
    recursive
    let remove = lam p. lam str. lam d.
      match str with "(*" ++ xs then remove (advanceCol p 2) xs (addi d 1) else
      match str with "\n" ++ xs then remove (advanceRow p 1) xs d else
      match str with "*)" ++ xs then
        if eqi d 1 then eatWSAC (advanceCol p 2) xs
        else remove (advanceCol p 2) xs (subi d 1) else
      match str with [_] ++ xs then remove (advanceCol p 1) xs d else
      if eqi d 0 then eatWSAC p str else posErrorExit p "Unmatched multiline comments."
    in remove (advanceCol p 2) xs 1
end

lang UnitTokenParser = TokenParser
  syn Token =
  | UnitTok {info : Info}
  syn TokenRepr =
  | UnitRepr ()

  sem parseToken (pos : Pos) =
  | "()" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = UnitTok {info = info}, lit = "()", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq (tokRepr : TokenRepr) =
  | UnitTok _ -> match tokRepr with UnitRepr _ then true else false
  sem tokInfo =
  | UnitTok {info = info} -> info

  sem tokReprToStr =
  | UnitRepr _ -> "<Unit>"
  sem tokToStr =
  | UnitTok _ -> "<Unit>"
  sem tokToRepr =
  | UnitTok _ -> UnitRepr ()
end

lang ArrayDelimTokenParser = TokenParser
  syn Token =
  | LArrTok {info : Info}
  | RArrTok {info : Info}
  syn TokenRepr =
  | LArrRepr ()
  | RArrRepr ()

  sem parseToken (pos : Pos) =
  | "[|" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = LArrTok {info = info}, lit = "[|", info = info, stream = {pos = pos2, str = str}}
  | "|]" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = RArrTok {info = info}, lit = "|]", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq (tokRepr : TokenRepr) =
  | LArrTok _ -> match tokRepr with LArrRepr _ then true else false
  | RArrTok _ -> match tokRepr with RArrRepr _ then true else false

  sem tokInfo =
  | LArrTok {info = info} -> info
  | RArrTok {info = info} -> info

  sem tokReprToStr =
  | LArrRepr _ -> "<LArr>"
  | RArrRepr _ -> "<RArr>"

  sem tokToStr =
  | LArrTok _ -> "<LArr>"
  | RArrTok _ -> "<RArr>"

  sem tokToRepr =
  | LArrTok _ -> LArrRepr ()
  | RArrTok _ -> RArrRepr ()
end

lang CharOrTickTokenParser = TokenParser
  syn Token =
  | CharTok {info : Info, val : Char}
  | TickTok {info : Info}
  syn TokenRepr =
  | CharRepr ()
  | TickRepr ()

  sem parseToken (pos : Pos) =
  | "'" ++ str ->
    let postTickPos = advanceCol pos 1 in
    let charRet = if null str then None () else
      match matchChar postTickPos str with {val = val, pos = pos2, str = str} in
      match str with "'" ++ str then
        let pos2 = advanceCol pos2 1 in
        let info = makeInfo pos pos2 in
        Some
        { token = CharTok {info = info, val = val}
        , lit = ""
        , info = info
        , stream = {pos = pos2, str = str}
        }
      else None () in
    match charRet with Some r then r else
    let info = makeInfo pos postTickPos in
    { token = TickTok {info = info}
    , lit = "'"
    , info = info
    , stream = {pos = postTickPos, str = str}
    }

  sem tokKindEq (tokRepr : TokenRepr) =
  | CharTok _ -> match tokRepr with CharRepr _ then true else false
  | TickTok _ -> match tokRepr with TickRepr _ then true else false

  sem tokInfo =
  | CharTok {info = info} -> info
  | TickTok {info = info} -> info

  sem tokReprToStr =
  | CharRepr _ -> "<Char>"
  | TickRepr _ -> "<Tick>"

  sem tokToStr =
  | CharTok tok -> snoc "<Char>" tok.val
  | TickTok tok -> snoc "<Tick>" tok.val

  sem tokToRepr =
  | CharTok _ -> CharRepr ()
  | TickTok _ -> TickRepr ()
end
