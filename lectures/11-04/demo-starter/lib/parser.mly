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
  | EOF { Var "x" }

(* <expr> ::= fun <var> -> <expr>
            | <expr2> { <expr2> }
*)

(* <expr2> ::= <var> | <int> | ( <expr> ) *)
