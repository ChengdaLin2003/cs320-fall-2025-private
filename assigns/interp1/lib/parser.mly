%{ open Utils %}

%token LET IN EQ PLUS STAR LPAREN RPAREN TRUE FALSE AND OR
%token <int> NUM
%token <string> VAR
%token EOF

%left OR
%left AND
%left PLUS
%left STAR

%start <Utils.expr> prog
%%
prog:
  | expr EOF { $1 }
;

expr:
  | LET VAR EQ expr IN expr { Let($2, $4, $6) }
  | expr OR expr            { Bop(Or,  $1, $3) }
  | expr AND expr           { Bop(And, $1, $3) }
  | expr PLUS expr          { Bop(Add, $1, $3) }
  | expr STAR expr          { Bop(Mul, $1, $3) }
  | atom                    { $1 }
;

atom:
  | TRUE                    { True }
  | FALSE                   { False }
  | NUM                     { Num $1 }
  | VAR                     { Var $1 }
  | LPAREN expr RPAREN      { $2 }
;