{
open Parser
}

let num = '-'?['0'-'9']+
let var = ['a'-'z']+
let whitespace = [' ' '\t' '\n' '\r']+

rule read =
  parse
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | var { IDENT (Lexing.lexeme lexbuf) }
  | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace { read lexbuf }
  | eof { EOF }