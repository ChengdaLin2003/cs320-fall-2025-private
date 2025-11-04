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

(* 左结合 & 优先级从低到高：OR, AND, 比较, + -, * / mod, (一元-), 应用, 原子 *)
%left OR
%left AND
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left APP

%%

prog:
  | expr EOF                          { $1 }

expr:
  | or_expr                           { $1 }

(* || 左结合 *)
or_expr:
  | or_expr OR and_expr               { Bop (Or,  $1, $3) }
  | and_expr                          { $1 }

(* && 左结合 *)
and_expr:
  | and_expr AND cmp_expr             { Bop (And, $1, $3) }
  | cmp_expr                          { $1 }

(* 比较（单次比较；若要链式可改为左递归 cmp_expr） *)
cmp_expr:
  | add_expr LT  add_expr             { Bop (Lt,  $1, $3) }
  | add_expr LE  add_expr             { Bop (Lte, $1, $3) }
  | add_expr GT  add_expr             { Bop (Gt,  $1, $3) }
  | add_expr GE  add_expr             { Bop (Gte, $1, $3) }
  | add_expr EQ  add_expr             { Bop (Eq,  $1, $3) }
  | add_expr NEQ add_expr             { Bop (Neq, $1, $3) }
  | add_expr                          { $1 }

(* + - 左结合 *)
add_expr:
  | add_expr PLUS  mul_expr           { Bop (Add, $1, $3) }
  | add_expr MINUS mul_expr           { Bop (Sub, $1, $3) }
  | mul_expr                          { $1 }

(* * / mod 左结合；操作数改为 u_expr（含一元负号） *)
mul_expr:
  | mul_expr STAR  u_expr             { Bop (Mul, $1, $3) }
  | mul_expr SLASH u_expr             { Bop (Div, $1, $3) }
  | mul_expr MOD   u_expr             { Bop (Mod, $1, $3) }
  | u_expr                            { $1 }

(* 一元负号层：把 -e 解析为 0 - e；放在应用之上，避免把 “-e” 当作原子参与应用 *)
u_expr:
  | MINUS u_expr                      { Bop (Sub, Num 0, $2) }
  | app_expr                          { $1 }

(* 应用：左结合，操作数必须是 primary（不以 MINUS 开头） *)
app_expr:
  | app_expr primary %prec APP        { App ($1, $2) }
  | primary                           { $1 }

(* 原子：不包含一元负号；let/if/fun/常量/变量/括号 *)
primary:
  | IF expr THEN expr ELSE expr       { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr           { Let ($2, $4, $6) }
  | FUN VAR ARROW expr                { Fun ($2, $4) }
  | NUM                               { Num $1 }
  | TRUE                              { True }
  | FALSE                             { False }
  | UNIT                              { Unit }
  | VAR                               { Var $1 }
  | LPAREN expr RPAREN                { $2 }