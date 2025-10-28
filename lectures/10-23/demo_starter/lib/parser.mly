%{
    open Utils
%}

%token LET
%token EQUALS
%token IN
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token<int> INT
%token<string> IDENT
%token EOF

%left PLUS, MINUS
%left STAR, SLASH

%start<Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | LET; x = IDENT; EQUALS;
    e1 = expr; IN; e2 = expr
    { Let (x, e1, e2) }
  | e = expr1 { e }

expr1:
  | e1 = expr1; op = bop; e2 = expr1
    { Bop (op, e1, e2) }
  | n = INT { Num n }
  | x = IDENT { Var x }
  | LPAREN; e = expr; RPAREN { e }

%inline bop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
