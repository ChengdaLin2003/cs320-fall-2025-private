%{ open Utils %}

%token LET IN EQ PLUS LPAREN RPAREN
%token <int> NUM
%token <string> VAR
%token EOF

%left PLUS      /* make + left-associative */

%start <Utils.expr> prog
%%
prog:
  | expr EOF { $1 }
;

expr:
  | LET VAR EQ expr IN expr { Let($2, $4, $6) }
  | expr PLUS expr          { Bop(Add, $1, $3) }
  | atom                    { $1 }
;

atom:
  | NUM                     { Num $1 }
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 }
;