%{ open Utils %}

%token LET IN IF THEN ELSE FUN TRUE FALSE
%token PLUS MINUS STAR SLASH LPAREN RPAREN AND OR LT
%token ARROW
%token EQ
%token <int> NUM
%token <string> VAR
%token EOF

%left OR
%left AND
%nonassoc LT
%left PLUS MINUS
%left STAR SLASH
%right UMINUS

%start <Utils.expr> prog
%%
prog:
  | expr EOF { $1 }
;

expr:
  | LET VAR EQ expr IN expr        { Let($2, $4, $6) }
  | IF expr THEN expr ELSE expr    { If($2, $4, $6) }
  | FUN VAR ARROW expr             { Fun($2, $4) }
  | or_expr                        { $1 }
;

or_expr:
  | or_expr OR and_expr            { Bop(Or,  $1, $3) }
  | and_expr                       { $1 }
;

and_expr:
  | and_expr AND rel_expr          { Bop(And, $1, $3) }
  | rel_expr                       { $1 }
;

rel_expr:
  | rel_expr LT add_expr           { Bop(Lt,  $1, $3) }
  | add_expr                       { $1 }
;

add_expr:
  | add_expr PLUS mul_expr         { Bop(Add, $1, $3) }
  | add_expr MINUS mul_expr        { Bop(Sub, $1, $3) }
  | mul_expr                       { $1 }
;

mul_expr:
  | mul_expr STAR unary_expr       { Bop(Mul, $1, $3) }
  | mul_expr SLASH unary_expr      { Bop(Div, $1, $3) }
  | unary_expr                     { $1 }
;

unary_expr:
  | MINUS unary_expr %prec UMINUS  { Bop(Sub, Num 0, $2) }
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