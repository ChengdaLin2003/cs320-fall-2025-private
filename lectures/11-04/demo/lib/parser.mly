%{
open Utils
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN
%token RPAREN
%token<string> VAR
%token<int> INT

%start <Utils.prog> prog

%%

(* <prog> ::= <expr> *)
prog:
  | e = expr; EOF { e }

(* <expr> ::= fun <var> -> <expr>
            | <expr2> { <expr2> }
*)
expr:
  | FUN; x = VAR; ARROW; e = expr { Fun (x, e) }
  | e = expr2; es = expr2* { List.fold_left (fun e1 e2 -> App (e1, e2)) e es }

(* <expr2> ::= <var> | <int> | ( <expr> ) *)
expr2:
  | x = VAR { Var x }
  | n = INT { Int n }
  | LPAREN; e = expr; RPAREN { e }
