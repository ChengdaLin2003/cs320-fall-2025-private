%{
open Utils

(* Build nested Fun nodes from argument list, optionally
   annotating the whole function with a result type. *)
let mk_func (ret_ty : ty option) (args : (ident * ty option) list) (body : expr) : expr =
  let body =
    match ret_ty with
    | None -> body
    | Some ty -> Annot (body, ty)
  in
  List.fold_right
    (fun (x, ty_opt) acc -> Fun (x, ty_opt, acc))
    args
    body

(* Build a list [h; e1; e2; ...] as Cons ... Nil. *)
let mk_list (h : expr) (es : expr list) : expr =
  let tl =
    List.fold_right
      (fun x acc -> Bop (Cons, x, acc))
      es
      Nil
  in
  Bop (Cons, h, tl)
%}

%token EOF
%token <int>   INT
%token <float> FLOAT
%token <string> VAR

(* keywords / punctuation *)
%token LET
%token REC
%token EQ
%token IN
%token COLON

%token FUN
%token MATCH
%token WITH
%token ALT
%token IF
%token THEN
%token ELSE

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token SEMICOLON
%token COMMA

%token TRUE
%token FALSE
%token SOME
%token NONE

(* type keywords *)
%token TUNIT
%token TINT
%token TFLOAT
%token TBOOL
%token TLIST
%token TOPTION
%token ARROW

(* binary operators *)
%token ADD
%token SUB
%token STAR
%token DIV
%token MOD
%token ADDF
%token SUBF
%token MULF
%token DIVF
%token POW
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR
%token CONS

%token ASSERT

(* precedence: taken from spec *)
%nonassoc TLIST
%nonassoc TOPTION
%right ARROW
%nonassoc COMMA
%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%right CONS
%left ADD ADDF SUB SUBF
%left STAR MULF DIV DIVF MOD
%left POW

%start <Utils.prog> prog

%%

prog:
  | tops=toplet*; EOF { tops }

toplet:
  | LET; rc=REC?; name=VAR; args=arg*; ret_ty=annot?; EQ; binding=expr
    {
      {
        is_rec = Option.is_some rc;
        name;
        binding = mk_func ret_ty args binding;
      }
    }

annot:
  | COLON; ty=ty { ty }

(* x        or   (x : ty) *)
arg:
  | x=VAR                          { (x, None) }
  | LPAREN; x=VAR; ty=annot; RPAREN { (x, Some ty) }

(* types, following spec grammar *)
ty:
  | TUNIT                { TUnit }
  | TINT                 { TInt }
  | TFLOAT               { TFloat }
  | TBOOL                { TBool }
  | tv=VAR               { TVar tv }              (* 'a, 'foo etc. *)
  | t=ty; TLIST          { TList t }
  | t=ty; TOPTION        { TOption t }
  | t1=ty; STAR; t2=ty   { TPair (t1, t2) }
  | t1=ty; ARROW; t2=ty  { TFun (t1, t2) }
  | LPAREN; t=ty; RPAREN { t }

(* expressions *)

expr:
  (* let / let rec in *)
  | LET; rc=REC?; name=VAR; args=arg*; ret_ty=annot?; EQ; binding=expr; IN; body=expr
    {
      Let
        {
          is_rec = Option.is_some rc;
          name;
          binding = mk_func ret_ty args binding;
          body;
        }
    }
  (* fun ... -> ... *)
  | FUN; args=arg+; ARROW; body=expr
    { mk_func None args body }
  (* if ... then ... else ... *)
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr
    { If (e1, e2, e3) }
  (* match e with | x, y -> e'  (pairs) *)
  | MATCH; e=expr; WITH; ALT; x=VAR; COMMA; y=VAR; ARROW; case_e=expr
    {
      PairMatch
        {
          matched = e;
          fst_name = x;
          snd_name = y;
          case = case_e;
        }
    }
  (* match e with | Some x -> e1 | None -> e2  (options) *)
  | MATCH; e=expr; WITH;
      ALT; SOME; x=VAR; ARROW; some_case=expr;
      ALT; NONE; ARROW; none_case=expr
    {
      OptMatch
        {
          matched = e;
          some_name = x;
          some_case;
          none_case;
        }
    }
  (* match e with | h :: t -> e1 | [] -> e2  (lists) *)
  | MATCH; e=expr; WITH;
      ALT; h=VAR; CONS; t=VAR; ARROW; cons_case=expr;
      ALT; LBRACKET; RBRACKET; ARROW; nil_case=expr
    {
      ListMatch
        {
          matched = e;
          hd_name = h;
          tl_name = t;
          cons_case;
          nil_case;
        }
    }
  | e=expr2
    { e }

(* binary operators, assert, Some, and application *)

%inline bop:
  | ADD  { Add }
  | SUB  { Sub }
  | STAR { Mul }
  | DIV  { Div }
  | MOD  { Mod }
  | ADDF { AddF }
  | SUBF { SubF }
  | MULF { MulF }
  | DIVF { DivF }
  | POW  { PowF }
  | LT   { Lt }
  | LTE  { Lte }
  | GT   { Gt }
  | GTE  { Gte }
  | EQ   { Eq }
  | NEQ  { Neq }
  | AND  { And }
  | OR   { Or }
  | COMMA { Comma }
  | CONS { Cons }

expr2:
  | e1=expr2; op=bop; e2=expr2
    { Bop (op, e1, e2) }
  | ASSERT; e=expr3
    { Assert e }
  | SOME; e=expr3
    { ESome e }
  (* application: e0 e1 e2 ... -> ((e0 e1) e2) ... *)
  | es=expr3+
    {
      List.(fold_left
              (fun acc x -> App (acc, x))
              (hd es)
              (tl es))
    }

(* helper for list literals with semicolons *)
list_item:
  | SEMICOLON; e=expr { e }

expr3:
  | LPAREN; RPAREN                     { Unit }
  | TRUE                               { Bool true }
  | FALSE                              { Bool false }
  | NONE                               { ENone }
  | LBRACKET; RBRACKET                 { Nil }
  | LBRACKET; e=expr; es=list_item*; RBRACKET
      { mk_list e es }
  | n=INT                              { Int n }
  | n=FLOAT                            { Float n }
  | x=VAR                              { Var x }
  | LPAREN; e=expr; RPAREN             { e }                   (* plain (e) *)
  | LPAREN; e=expr; annot=annot; RPAREN { Annot (e, annot) }   (* (e : ty) *)