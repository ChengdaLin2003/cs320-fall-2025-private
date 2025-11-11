{
  open Parser

  (* ------------------------------------------------------ *)
  (* Keyword recognition: returns Some <token> if the       *)
  (* identifier matches a reserved word, otherwise None.    *)
  (* ------------------------------------------------------ *)
  let keyword = function
    | "let"   -> Some LET
    | "in"    -> Some IN
    | "if"    -> Some IF
    | "then"  -> Some THEN
    | "else"  -> Some ELSE
    | "fun"   -> Some FUN
    | "rec"   -> Some REC
    | "true"  -> Some TRUE
    | "false" -> Some FALSE
    | _ -> None
}

(* ------------------------------------------------------ *)
(* Main lexing rule: read successive tokens from input.   *)
(* The first matching pattern is chosen, so order matters *)
(* (multi-character symbols must appear before single ones). *)
(* ------------------------------------------------------ *)
rule read = parse

  (* ---------- Whitespace and Comments ---------- *)
  | [' ' '\t' '\r' '\n']+        { read lexbuf }    (* Skip all whitespace *)
  | "//" [^'\n']*               { read lexbuf }     (* Single-line comments: '//' ... newline *)
  | "(*"                        { comment 1 lexbuf }(* Multi-line / nested comments start *)

  (* ---------- Multi-character operators ---------- *)
  | "||"                        { OR }
  | "&&"                        { AND }
  | "<="                        { LE }
  | ">="                        { GE }
  | "<>"                        { NEQ }
  | "->"                        { ARROW }
  | "()"                        { UNIT }

  (* ---------- Single-character operators / punctuation ---------- *)
  | "<"                         { LT }
  | ">"                         { GT }
  | "+"                         { PLUS }
  | "-"                         { MINUS }
  | "*"                         { STAR }
  | "/"                         { SLASH }
  | "="                         { EQ }
  | "("                         { LPAREN }
  | ")"                         { RPAREN }

  (* ---------- Numeric literals ---------- *)
  | ['0'-'9']+ as n             { NUM (int_of_string n) }
      (* Only non-negative digits here; unary '-' handled in parser. *)

  (* ---------- Identifiers and Keywords ---------- *)
  | "mod"                       { MOD }
  | ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as id {
      match keyword id with
      | Some kw -> kw          (* reserved keyword *)
      | None    -> VAR id      (* otherwise a variable name *)
    }

  (* ---------- End of input ---------- *)
  | eof                         { EOF }

  (* ---------- Catch-all: skip any unrecognized character ---------- *)
  | _                           { read lexbuf }


(* ------------------------------------------------------ *)
(* Nested comment handling: supports (* ... (* ... *) ... *) *)
(* ------------------------------------------------------ *)
and comment depth = parse
  | "(*"                        { comment (depth + 1) lexbuf }  (* new nested level *)
  | "*)"                        { if depth = 1 then read lexbuf (* leave last level *)
                                  else comment (depth - 1) lexbuf }
  | eof                         { failwith "Unterminated comment" }
  | _                           { comment depth lexbuf }         (* consume anything inside *)