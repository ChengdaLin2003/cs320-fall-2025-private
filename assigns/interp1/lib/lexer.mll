{
  open Parser

  (* 关键字映射 *)
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
  (* 空白与单行注释 *)
  | [' ' '\t' '\r' '\n']+        { read lexbuf }
  | "//" [^'\n']*               { read lexbuf }
  | "(*"                        { comment 1 lexbuf }

  (* 多字符运算/记号必须在前面 *)
  | "||"                        { OR }
  | "&&"                        { AND }
  | "<="                        { LE }
  | ">="                        { GE }
  | "<>"                        { NEQ }
  | "->"                        { ARROW }
  | "()"                        { UNIT }

  (* 单字符运算/括号 *)
  | "<"                         { LT }
  | ">"                         { GT }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | "*"                         { STAR }
  | "/"                         { SLASH }
  | "="                         { EQ }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }

  (* 数字：仅 digits；负数用语法表示如 (-5) *)
  | ['0'-'9']+ as n             { NUM (int_of_string n) }

  (* “mod” 运算符；其他标识符看是否关键字，否则为 VAR *)
  | "mod"                       { MOD }
  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as id {
      match keyword id with
      | Some kw -> kw
      | None    -> VAR id
    }

  | eof                         { EOF }
  | _                           { read lexbuf }

and comment depth = parse
  | "(*"                        { comment (depth + 1) lexbuf }
  | "*)"                        { if depth = 1 then read lexbuf else comment (depth - 1) lexbuf }
  | eof                         { failwith "Unterminated comment" }
  | _                           { comment depth lexbuf }