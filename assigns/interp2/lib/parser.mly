%{
open Utils

(* 只保留柯里化 Fun 的 helper；不要再构造函数类型 *)
let fun_chain args body =
  List.fold_right (fun (x,t) acc -> Fun (x,t,acc)) args body
%}

%token LET REC IN IF THEN ELSE FUN TRUE FALSE ASSERT
%token INTKW BOOLKW UNITKW MODKW
%token PLUS MINUS STAR SLASH LT LTE GT GTE EQ NEQ AND OR ARROW
%token LPAREN RPAREN COLON
%token <int> NUM
%token <string> VAR
%token EOF

%start parse
%type <prog> parse

%%

parse:
  | EOF                                { [] }
  | toplet_list EOF                    { $1 }

toplet_list:
  | toplet                             { [$1] }
  | toplet toplet_list                 { $1 :: $2 }

(* 顶层：let [rec] f (x1:t1) ... (xk:tk) : tr = e *)
toplet:
  | LET rec_opt VAR arg_list_opt COLON ty EQ expr
      { { is_rec = $2; name = $3; args = $4; ann = $6; body = $8 } }

rec_opt:
  | REC                                { true }
  |                                    { false }

arg_list_opt:
  |                                    { [] }
  | arg arg_list_opt                   { $1 :: $2 }

arg:
  | LPAREN VAR COLON ty RPAREN         { ($2, $4) }

ty:
  | INTKW                              { TInt }
  | BOOLKW                             { TBool }
  | UNITKW                             { TUnit }
  | ty ARROW ty                        { TFun ($1, $3) }
  | LPAREN ty RPAREN                   { $2 }

expr:
  (* let [rec] f (x1:t1)...(xk:tk) : tr = e1 in e2
     这里我们：
       - 把 (x1:t1)...(xk:tk) 柯里化成 Fun 链
       - 但类型字段直接用注解 $6（tr），不再自己构造箭头类型
  *)
  | LET rec_opt VAR arg_list_opt COLON ty EQ expr IN expr
      { let fun_e = fun_chain $4 $8 in
        if $2 then LetRec ($3, $6, fun_e, $10)
        else       Let    ($3, $6, fun_e, $10) }

  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }

  | FUN arg arg_list_opt ARROW expr
      { fun_chain ($2 :: $3) $5 }

  | bop_expr                           { $1 }

bop_expr:
  | bop_expr PLUS bop_expr             { Bop (Add, $1, $3) }
  | bop_expr MINUS bop_expr            { Bop (Sub, $1, $3) }
  | bop_expr STAR bop_expr             { Bop (Mul, $1, $3) }
  | bop_expr SLASH bop_expr            { Bop (Div, $1, $3) }
  | bop_expr MODKW bop_expr            { Bop (Mod, $1, $3) }
  | bop_expr LT bop_expr               { Bop (Lt, $1, $3) }
  | bop_expr LTE bop_expr              { Bop (Lte, $1, $3) }
  | bop_expr GT bop_expr               { Bop (Gt, $1, $3) }
  | bop_expr GTE bop_expr              { Bop (Gte, $1, $3) }
  | bop_expr EQ bop_expr               { Bop (Eq, $1, $3) }
  | bop_expr NEQ bop_expr              { Bop (Neq, $1, $3) }
  | bop_expr AND bop_expr              { Bop (And, $1, $3) }
  | bop_expr OR bop_expr               { Bop (Or, $1, $3) }
  | app_expr                           { $1 }

app_expr:
  | app_expr simple_expr               { App ($1, $2) }
  | ASSERT simple_expr                 { Assert $2 }
  | simple_expr                        { $1 }

simple_expr:
  | LPAREN expr RPAREN                 { $2 }
  | TRUE                               { True }
  | FALSE                              { False }
  | UNITKW                             { Unit }
  | NUM                                { Num $1 }
  | VAR                                { Var $1 }