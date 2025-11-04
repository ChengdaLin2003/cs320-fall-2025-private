%{
(* Bring in AST type definitions from Utils. *)
open Utils
%}

(* ------------------------------------------------------ *)
(* Token Declarations                                    *)
(* ------------------------------------------------------ *)
%token <int> NUM              /* integer literal */
%token <string> VAR           /* variable name */
%token TRUE FALSE             /* boolean literals */
%token LET IN IF THEN ELSE    /* keywords for let / if */
%token FUN ARROW              /* function keyword and arrow "->" */
%token PLUS MINUS STAR SLASH MOD   /* arithmetic operators */
%token LT LE GT GE EQ NEQ          /* comparison operators */
%token AND OR                      /* logical operators */
%token UNIT                        /* "()" literal */
%token LPAREN RPAREN               /* parentheses */
%token EOF                         /* end of input marker */

(* ------------------------------------------------------ *)
(* Entry point and return type                            *)
(* ------------------------------------------------------ *)
%start prog
%type <expr> prog

(* ------------------------------------------------------ *)
(* Operator Precedence and Associativity                  *)
(* Lower lines = higher precedence                        *)
(* ------------------------------------------------------ *)
%left OR                      /* lowest precedence */
%left AND
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left APP                     /* function application (highest) */

%%

(* ====================================================== *)
(* Grammar Rules                                           *)
(* Each nonterminal builds an AST node from subexpressions. *)
(* ====================================================== *)

(* ------------------------------------------------------ *)
(* Top-level: program is a single expression followed by EOF *)
(* ------------------------------------------------------ *)
prog:
  | expr EOF                          { $1 }

(* Entry point for expressions *)
expr:
  | or_expr                           { $1 }

(* ---------- Boolean OR ---------- *)
(* Left-associative: (a || b || c) => (Or (Or a b) c) *)
or_expr:
  | or_expr OR and_expr               { Bop (Or,  $1, $3) }
  | and_expr                          { $1 }

(* ---------- Boolean AND ---------- *)
(* Left-associative, higher precedence than OR *)
and_expr:
  | and_expr AND cmp_expr             { Bop (And, $1, $3) }
  | cmp_expr                          { $1 }

(* ---------- Comparisons ---------- *)
(* Left-associative (for chained comparisons, left nest). *)
cmp_expr:
  | cmp_expr LT  add_expr             { Bop (Lt,  $1, $3) }
  | cmp_expr LE  add_expr             { Bop (Lte, $1, $3) }
  | cmp_expr GT  add_expr             { Bop (Gt,  $1, $3) }
  | cmp_expr GE  add_expr             { Bop (Gte, $1, $3) }
  | cmp_expr EQ  add_expr             { Bop (Eq,  $1, $3) }
  | cmp_expr NEQ add_expr             { Bop (Neq, $1, $3) }
  | add_expr                          { $1 }

(* ---------- Addition / Subtraction ---------- *)
(* Left-associative: (1 - 2 - 3) => (Sub (Sub 1 2) 3) *)
add_expr:
  | add_expr PLUS  mul_expr           { Bop (Add, $1, $3) }
  | add_expr MINUS mul_expr           { Bop (Sub, $1, $3) }
  | mul_expr                          { $1 }

(* ---------- Multiplication / Division / Mod ---------- *)
(* Left-associative and binds tighter than +/-. 
   The right operand is a u_expr so that unary '-' binds first. *)
mul_expr:
  | mul_expr STAR  u_expr             { Bop (Mul, $1, $3) }
  | mul_expr SLASH u_expr             { Bop (Div, $1, $3) }
  | mul_expr MOD   u_expr             { Bop (Mod, $1, $3) }
  | u_expr                            { $1 }

(* ---------- Unary Operators ---------- *)
(* Handles unary minus both for numeric literals (-3)
   and general expressions (-e = 0 - e). 
   Placed above application to avoid mis-parsing as App(1, -2). *)
u_expr:
  | MINUS NUM                         { Num (- $2) }             /* literal negative number */
  | MINUS u_expr                      { Bop (Sub, Num 0, $2) }   /* general negation */
  | app_expr                          { $1 }

(* ---------- Function Application ---------- *)
(* Left-associative: f x y = App(App(f,x),y). 
   Has highest precedence, below unary ops. *)
app_expr:
  | app_expr primary %prec APP        { App ($1, $2) }
  | primary                           { $1 }

(* ---------- Atomic Expressions ---------- *)
(* Core syntactic forms that cannot be decomposed further. *)
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