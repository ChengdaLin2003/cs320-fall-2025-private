%{ open Utils %}

%token LET IN IF THEN ELSE FUN TRUE FALSE
%token PLUS STAR LPAREN RPAREN AND OR LT
%token ARROW
%token EQ
%token <int> NUM
%token <string> VAR
%token EOF

%left OR
%left AND
%nonassoc LT
%left PLUS
%left STAR

%start <Utils.expr> prog
%%
prog:
  | expr EOF { $1 }
;

expr:
  | LET VAR EQ expr IN expr        { Let($2, $4, $6) }
  | IF expr THEN expr ELSE expr    { If($2, $4, $6) }
  | FUN VAR ARROW expr             { Fun($2, $4) }
  | expr OR expr                   { Bop(Or,  $1, $3) }
  | expr AND expr                  { Bop(And, $1, $3) }
  | expr LT expr                   { Bop(Lt,  $1, $3) }
  | expr PLUS expr                 { Bop(Add, $1, $3) }
  | expr STAR expr                 { Bop(Mul, $1, $3) }
  | app_expr                       { $1 }
;

app_expr:
  | app_expr atom                  { App($1, $2) }
  | atom                           { $1 }
;

atom:
  | TRUE                           { True }
  | FALSE                          { False }
  | NUM                            { Num $1 }
  | VAR                            { Var $1 }
  | LPAREN expr RPAREN             { $2 }
;