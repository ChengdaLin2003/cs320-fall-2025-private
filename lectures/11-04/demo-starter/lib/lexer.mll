{
open Parser
}

let whitespace = [' ' '\n' '\t' '\r']+
let var = ['a'-'z']+
let int = '-'? ['0'-'9']+

rule read =
  parse
  | "fun" { FUN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | var { VAR (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace { read lexbuf }
  | eof { EOF }
