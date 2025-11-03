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
  | "//" [^'\n']*                { read lexbuf }
  | "(*"                          { comment 0 lexbuf }

  | "||"                           { OR }
  | "&&"                           { AND }
  | "->"                           { ARROW }
  | "<"                            { LT }
  | "+"                            { PLUS }
  | "*"                            { STAR }
  | "="                            { EQ }
  | "("                            { LPAREN }
  | ")"                            { RPAREN }

  | ['0'-'9']+ as n                { NUM (int_of_string n) }
  | ['a'-'z' 'A'-'Z' '_' '\'']['a'-'z' 'A'-'Z' '_' '0'-'9' '\'']* as id {
      match keyword id with
      | Some kw -> kw
      | None    -> VAR id
    }

  | eof                            { EOF }
  | _ as c                         { failwith (Printf.sprintf "Unexpected character: %C" c) }

and comment depth = parse
  | "(*"                           { comment (depth + 1) lexbuf }
  | "*)"                           { if depth = 0 then read lexbuf else comment (depth - 1) lexbuf }
  | eof                            { failwith "Unterminated comment" }
  | _                              { comment depth lexbuf }