%{
open Utils

let mk_func ty args body =
  let body =
    match ty with
    | None -> body
    | Some ty -> Annot (body, ty)
  in
  List.fold_right
    (fun (x, ty) acc -> Fun (x, ty, acc))
    args
    body

let mk_list h es =
  let tl =
    List.fold_right
      (fun x acc -> Bop (Cons, x, acc))
      es
      Nil
  in
  Bop (Cons, h, tl)
%}

%token EOF
%token <int> INT
%token <float> FLOAT
%token <string> VAR

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

%token TUNIT
%token TINT
%token TFLOAT
%token TBOOL
%token TLIST
%token TOPTION
%token <string> TVAR
%token ARROW

%token TRUE
%token FALSE

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
%token CONS
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR
%token COMMA

%token SOME
%token NONE
%token ASSERT

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
  | ls = toplet* EOF { ls }

toplet:
  | LET; rc=REC?; name=VAR; args=arg*; ty=annot?; EQ; binding=expr
    { {
        is_rec = Option.is_some rc;
        name;
        binding = mk_func ty args binding;
      }
    }

annot:
  | COLON; ty=ty { ty }


ty:
  | TUNIT              { TUnit }
  | TINT               { TInt }
  | TFLOAT             { TFloat }
  | TBOOL              { TBool }
  | v=TVAR             { TVar v }
  | ty TLIST           { TList   $1 }
  | ty TOPTION         { TOption $1 }
  | ty STAR ty         { TPair  ($1, $3) }
  | ty ARROW ty        { TFun   ($1, $3) }
  | LPAREN ty RPAREN   { $2 }


arg:
  | x=VAR                         { (x, None) }
  | LPAREN; x=VAR; ty=annot; RPAREN { (x, Some ty) }


expr:
  (* let [rec] x args [: ty] = e1 in e2 *)
  | LET; rc=REC?; name=VAR; args=arg*; ty=annot?; EQ; binding=expr; IN; body=expr
    { Let
        {
          is_rec = Option.is_some rc;
          name;
          binding = mk_func ty args binding;
          body;
        }
    }
  (* fun args -> body *)
  | FUN; args=arg*; ARROW; body=expr
    { mk_func None args body }
  (* if e1 then e2 else e3 *)
  | IF; c=expr; THEN; t_branch=expr; ELSE; e_branch=expr
    { If (c, t_branch, e_branch) }
  (* match e with | x, y -> e' *)
  | MATCH; scrut=expr; WITH; ALT; x=VAR; COMMA; y=VAR; ARROW; case_e=expr
    { PairMatch
        {
          matched = scrut;
          fst_name = x;
          snd_name = y;
          case = case_e;
        }
    }
  (* match e with | Some x -> e1 | None -> e2 *)
  | MATCH; scrut=expr; WITH; ALT; SOME; x=VAR; ARROW; some_e=expr;
           ALT; NONE; ARROW; none_e=expr
    { OptMatch
        {
          matched = scrut;
          some_name = x;
          some_case = some_e;
          none_case = none_e;
        }
    }
  (* match e with | h :: t -> e1 | [] -> e2 *)
  | MATCH; scrut=expr; WITH; ALT; h=VAR; CONS; t=VAR; ARROW; cons_e=expr;
           ALT; LBRACKET; RBRACKET; ARROW; nil_e=expr
    { ListMatch
        {
          matched = scrut;
          hd_name = h;
          tl_name = t;
          cons_case = cons_e;
          nil_case = nil_e;
        }
    }
  | e = expr2 { e }


%inline bop:
  | ADD   { Add }
  | SUB   { Sub }
  | STAR  { Mul }
  | DIV   { Div }
  | MOD   { Mod }
  | ADDF  { AddF }
  | SUBF  { SubF }
  | MULF  { MulF }
  | DIVF  { DivF }
  | POW   { PowF }
  | CONS  { Cons }
  | LT    { Lt }
  | LTE   { Lte }
  | GT    { Gt }
  | GTE   { Gte }
  | EQ    { Eq }
  | NEQ   { Neq }
  | AND   { And }
  | OR    { Or }
  | COMMA { Comma }

expr2:
  | e1=expr2; op=bop; e2=expr2 { Bop (op, e1, e2) }
  | ASSERT; e=expr3           { Assert e }
  | SOME; e=expr3             { ESome e }
  | es=expr3+
    { List.fold_left
        (fun acc x -> App (acc, x))
        (List.hd es)
        (List.tl es)
    }


list_item:
  | SEMICOLON; e=expr { e }

expr3:
  | LPAREN; RPAREN             { Unit }
  | LPAREN; e=expr; RPAREN     { e }
  | TRUE                       { Bool true }
  | FALSE                      { Bool false }
  | NONE                       { ENone }
  | LBRACKET; RBRACKET         { Nil }
  | LBRACKET; e=expr; es=list_item*; RBRACKET
    { mk_list e es }
  | n=INT                      { Int n }
  | n=FLOAT                    { Float n }
  | x=VAR                      { Var x }