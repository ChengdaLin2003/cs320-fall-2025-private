%{
open Utils

(* Helper: call-by-value fix-combinator as an AST.
   fix = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v)) *)
let fix_expr : expr =
  let f = "f" and x = "x" and v = "v" in
  let inner = Fun (v, App (App (Var x, Var x), Var v)) in
  let fx = Fun (x, App (Var f, inner)) in
  App (fx, fx)
%}

(* ---------- Tokens ---------- *)
%token <int> NUM
%token <string> VAR
%token TRUE FALSE
%token LET IN IF THEN ELSE REC
%token FUN ARROW
%token PLUS MINUS STAR SLASH MOD
%token LT LE GT GE EQ NEQ
%token AND OR
%token LPAREN RPAREN UNIT
%token EOF

(* ---------- Entry ---------- *)
%start prog
%type <expr> prog

(* ---------- Precedence (lowest first) ---------- *)
%left OR
%left AND
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left APP

%%
prog:
  expr EOF { $1 }

/* Expression with infix operators; precedence controlled above */
expr:
  | expr OR expr            { Bop (Or,  $1, $3) }
  | expr AND expr           { Bop (And, $1, $3) }
  | expr LT expr            { Bop (Lt,  $1, $3) }
  | expr LE expr            { Bop (Lte, $1, $3) }
  | expr GT expr            { Bop (Gt,  $1, $3) }
  | expr GE expr            { Bop (Gte, $1, $3) }
  | expr EQ expr            { Bop (Eq,  $1, $3) }
  | expr NEQ expr           { Bop (Neq, $1, $3) }
  | expr PLUS expr          { Bop (Add, $1, $3) }
  | expr MINUS expr         { Bop (Sub, $1, $3) }
  | expr STAR expr          { Bop (Mul, $1, $3) }
  | expr SLASH expr         { Bop (Div, $1, $3) }
  | expr MOD expr           { Bop (Mod, $1, $3) }
  | noninfix                { $1 }

/* Non-infix forms bind most loosely */
noninfix:
  | IF expr THEN expr ELSE expr        { If ($2, $4, $6) }
  | LET VAR EQ expr IN expr            { Let ($2, $4, $6) }
  | LET REC VAR EQ expr IN expr        { Let ($3, App (fix_expr, Fun ($3, $5)), $7) }
  | FUN VAR ARROW expr                 { Fun ($2, $4) }
  | app_expr                           { $1 }
  | primary                           { $1 }

/* Application only chains ATOMs (no leading unary). */
app_expr:
  | app_expr atom %prec APP            { App ($1, $2) }
  | atom                               { $1 }

/* Atom excludes unary minus to avoid "1 (-2)"-style accidental application. */
atom:
  | NUM                                { Num $1 }
  | TRUE                               { True }
  | FALSE                              { False }
  | UNIT                               { Unit }
  | VAR                                { Var $1 }
  | LPAREN expr RPAREN                 { $2 }

/* Unary minus builds on atom and has higher precedence than infix, but cannot be a callee. */
primary:
  | MINUS atom                         { Bop (Sub, Num 0, $2) }
  | atom                               { $1 }

