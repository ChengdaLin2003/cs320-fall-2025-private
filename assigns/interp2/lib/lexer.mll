{
open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n']     { token lexbuf }
  | "(*"                     { comment 1 lexbuf; token lexbuf }  (* 嵌套注释 *)

  | "let"                    { LET }
  | "rec"                    { REC }
  | "in"                     { IN }
  | "if"                     { IF }
  | "then"                   { THEN }
  | "else"                   { ELSE }
  | "fun"                    { FUN }
  | "true"                   { TRUE }
  | "false"                  { FALSE }
  | "assert"                 { ASSERT }

  | "int"                    { INTKW }
  | "bool"                   { BOOLKW }
  | "unit"                   { UNITKW }
  | "mod"                    { MODKW }

  | "->"                     { ARROW }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }
  | ':'                      { COLON }

  | '='                      { EQ }
  | "<>"                     { NEQ }
  | "<="                     { LTE }
  | "<"                      { LT }
  | ">="                     { GTE }
  | ">"                      { GT }
  | "&&"                     { AND }
  | "||"                     { OR }

  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { STAR }
  | '/'                      { SLASH }

  | ['0'-'9']+ as n          { NUM (int_of_string n) }
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as v { VAR v }

  | eof                      { EOF }
  | _                        { failwith "Unrecognized character" }

and comment depth = parse
  | "(*"  { comment (depth + 1) lexbuf }
  | "*)"  { if depth = 1 then () else comment (depth - 1) lexbuf }
  | eof   { failwith "Unclosed comment" }
  | _     { comment depth lexbuf }