{
  open Parser
  exception Error of string
}

let space = [' ' '\t' '\r' '\n']

rule read = parse
  | space+        { read lexbuf }
  | "let"         { LET }
  | "in"          { IN }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "&&"          { AND }
  | "||"          { OR }
  | '='           { EQ }
  | '+'           { PLUS }
  | '*'           { STAR }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '1'           { NUM 1 }
  | '2'           { NUM 2 }
  | '3'           { NUM 3 }
  | 'x'           { VAR "x" }
  | 'y'           { VAR "y" }
  | 'z'           { VAR "z" }
  | eof           { EOF }
  | _             { raise (Error "lex error") }