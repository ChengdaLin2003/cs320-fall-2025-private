%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token TRUE FALSE
%token LET IN IF THEN ELSE FUN ARROW
%token PLUS MINUS STAR SLASH MOD
%token LT LE GT GE EQ NEQ
%token AND OR
%token UNIT
%token LPAREN RPAREN
%token EOF

%start prog
%type <expr> prog

%right OR
%right AND
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left APP 

%%

prog:
  | expr EOF                          { $1 }

expr:
  | or_expr                           { $1 }

or_expr:
  | or_expr OR and_expr               { Bop (Or,  $1, $3) }
  | and_expr                          { $1 }

and_expr:
  | and_expr AND cmp_expr             { Bop (And, $1, $3) }
  | cmp_expr                          { $1 }

cmp_expr:
  | add_expr LT  add_expr             { Bop (Lt,  $1, $3) }
  | add_expr LE  add_expr             { Bop (Lte, $1, $3) }
  | add_expr GT  add_expr             { Bop (Gt,  $1, $3) }
  | add_expr GE  add_expr             { Bop (Gte, $1, $3) }
  | add_expr EQ  add_expr             { Bop (Eq,  $1, $3) }
  | add_expr NEQ add_expr             { Bop (Neq, $1, $3) }
  | add_expr                          { $1 }

add_expr:
  | add_expr PLUS mul_expr            { Bop (Add, $1, $3) }
  | add_expr MINUS mul_expr           { Bop (Sub, $1, $3) }
  | mul_expr                          { $1 }

mul_expr:
  | mul_expr STAR app_expr            { Bop (Mul, $1, $3) }
  | mul_expr SLASH app_expr           { Bop (Div, $1, $3) }
  | mul_expr MOD app_expr             { Bop (Mod, $1, $3) }
  | app_expr                          { $1 }

app_expr:
  | app_expr atom %prec APP           { App ($1, $2) } 
  | atom                              { $1 }

atom:
  | IF expr THEN expr ELSE expr       { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr           { Let ($2, $4, $6) }
  | FUN VAR ARROW expr                { Fun ($2, $4) }
  | MINUS NUM                         { Num (- $2) }
  | MINUS atom                        { Bop (Sub, Num 0, $2) }
  | NUM                               { Num $1 }
  | TRUE                              { True }
  | FALSE                             { False }
  | UNIT                              { Unit }
  | VAR                               { Var $1 }
  | LPAREN expr RPAREN                { $2 }