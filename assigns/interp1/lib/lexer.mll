{
  open Parser

  let keyword = function
    | "let"   -> Some LET
    | "in"    -> Some IN
    | "if"    -> Some IF
    | "then"  -> Some THEN
    | "else"  -> Some ELSE
    | "fun"   -> Some FUN
    | "true"  -> Some TRUE
    | "false" -> Some FALSE
    | _ -> None
}

rule read = parse
  | [' ' '\t' '\r' '\n']        { read lexbuf }
  | "//" [^'\n']*               { read lexbuf }
  | "(*"                        { comment 0 lexbuf }

  (* multi-char must come before single-char *)
  | "||"                        { OR }
  | "&&"                        { AND }
  | "->"                        { ARROW }

  | "<"                         { LT }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | "*"                         { STAR }
  | "/"                         { SLASH }
  | "="                         { EQ }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }

  | ['0'-'9']+ as n             { NUM (int_of_string n) }

  (* single leading apostrophe -> produce a token that parser won't consume *)
  | "'"                         { APOSTROPHE }

  (* identifiers: first char cannot be apostrophe; subsequent chars may include it *)
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']* as id {
      match keyword id with
      | Some kw -> kw
      | None    -> VAR id
    }

  | eof                         { EOF }
  (* for any other unexpected char, skip it to let parser fail gracefully *)
  | _                           { read lexbuf }

and comment depth = parse
  | "(*"                        { comment (depth + 1) lexbuf }
  | "*)"                        { if depth = 0 then read lexbuf else comment (depth - 1) lexbuf }
  | eof                         { failwith "Unterminated comment" }
  | _                           { comment depth lexbuf }