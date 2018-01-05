%{
open Lang

exception Internal_error of string

let rec ctor_of_int (n:int) : exp =
  if n = 0
  then ECtor("O", [])
  else ECtor("S", [ctor_of_int (n-1)])

let rec list_of_exps (l:exp list) : exp = EList l
(*  match l with
  | [] -> ECtor("Nil", [])
  | e::es -> ECtor("Cons", [e; (list_of_exps es)])
*)
let rec appify (e:exp) (es:exp list) : exp =
  match es with
  | [] -> e
  | e'::es -> appify (EApp (e, e')) es

let rec binding_args : arg list -> exp -> exp
= fun args e ->
  match args with
  | [] -> e
  | hd::tl -> EFun (hd, binding_args tl e)
%}

%token <string> LID   (* lowercase identifiers *)
%token <string> UID   (* uppercase identifiers *)
%token <int> INT      (* integer constants translated to O, S(O), S(S(O)), etc. *)
%token <string> STRING

%token FUN        (* fun *)
%token MATCH      (* match *)
%token WITH       (* with *)
%token TYPE       (* type *)
%token OF         (* of *)
%token LET        (* let *)
%token IN         (* in *)
%token REC        (* rec *)
%token IF         (* if *)
%token ELSE       (* else *)
%token THEN       (* then *)
%token NOT        (* not *)
%token TRUE
%token FALSE
%token TBool
%token TInt
%token TList
%token TString

%token HOLE       (* ? *)
(* %token IMPLIES    (* |> *) *)
%token EQ         (* = *)
%token ARR        (* -> *)
(* %token FATARR     (* => *) *)
%token COMMA      (* , *)
%token COLON      (* : *)
%token SEMI       (* ; *)
%token STAR       (* * *)
%token PIPE       (* | *)
%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token LBRACE     (* { *)
%token RBRACE     (* } *)
%token LBRACKET   (* [ *)
%token RBRACKET   (* ] *)

%token UNDERBAR   (* _ *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token DIVIDE     (* / *)
%token MOD        (* % *)

%token OR         (* || *)
%token AND        (* && *)
%token LESS       (*  *)
%token LARGER     (* > *)
%token LESSEQ     (* *)
%token LARGEREQ   (* >= *) 
%token NOTEQ      (* != or <> *)

%token AT         (* @ *)
%token DOUBLECOLON(* :: *)


%token EOF

%nonassoc ASSIGN
%left OR
%left AND
%left LESS LESSEQ LARGER LARGEREQ EQ NOTEQ
%right AT
%right DOUBLECOLON
%left PLUS MINUS
%left STAR DIVIDE MOD
%left PIPE
%right UNARY

%start prog
%type <Lang.examples * Lang.prog> prog
%%

prog:
  | ds=decls EOF
    { ([],(List.rev ds)) }
  | LBRACE es=examples RBRACE ds=decls EOF
    { (es,(List.rev ds)) }

examples:
  | e= example
    { [e] }
  | e= example es=examples
    { e:: es}

example:
  | i=exp_list ARR o=value SEMI
    { (i,o) }

exp_list:
  | e=exp
    { [e] }
  | e=exp COMMA el=exp_list
    { e::el }

value:
  | c= value_base
    { c }
  | LPAREN c=value_base RPAREN
    { c }
  | LPAREN c=value_comma_list RPAREN
    { VTuple(c) }
  | LBRACKET c=value_list RBRACKET
    { VList(c) }

value_base:
  | c=INT
    { VInt (c) }
  | TRUE
    { VBool (true) }
  | FALSE
    { VBool (false) }
  | c=UID
    { VCtor (c, []) }
  | c=UID LPAREN cl=value_comma_list RPAREN
    { VCtor (c, cl) }
  | c=STRING
    { VString (c) }
  | MINUS c=INT
    { VInt (-c)}

value_list : 
  | (*empty*)
    { [] }
  | c=value
    { [c] }
  | c=value_base SEMI cl=value_list_one
    { c::cl }

value_list_one :
  | c=value_base
    { [c] }
  | c= value_base SEMI cl = value_list_one
    { c:: cl}

value_comma_list : 
  | (*empty*)
    { [] }
  | c=value
    { [c] }
  | c=value_base COMMA cl=value_comma_list_one
    { c::cl }

value_comma_list_one :
  | c=value_base
    { [c] }
  | c= value_base COMMA cl = value_comma_list_one
    { c:: cl}

(***** Declarations {{{ *****)

decls:  (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | ds=decls d=datatype
    { d::ds }
  | ds=decls l=letbind
    { l::ds }

datatype:
  | TYPE d=LID EQ cs=ctors
    { DData (d, List.rev cs) }

letbind:
  | LET x=LID COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, false, [], t, e) }
  | LET x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI (*arg => (x:t)*)
    { DLet (x, false, List.rev args, t, e) }
  | LET REC x=LID args=arg_list COLON t=typ EQ e=exp SEMI SEMI
    { DLet (x, true, List.rev args, t, e) }
  | LET x=LID EQ e=exp SEMI SEMI
    { DLet (x, false, [], TPoly, e) }
  | LET x=LID args=arg_list EQ e=exp SEMI SEMI (*arg => (x:t)*)
    { DLet (x, false, List.rev args, TPoly, e) }
  | LET REC x=LID args=arg_list EQ e=exp SEMI SEMI
    { DLet (x, true, List.rev args, TPoly, e) }
  | e=exp SEMI SEMI
    { DLet ("@", false, [], TPoly, e)}

ctors:  (* NOTE: reversed *)
  | (* empty *)
    { [] }
  | cs=ctors c=ctor
    { c::cs }

ctor:
  | PIPE c=UID OF ts=star_typ_list
    { (c, List.rev ts) }
  | PIPE c=UID
    { (c, []) }

star_typ_list:  (* NOTE: reversed *)
  | t=typ
    { [t] }
  | ts=star_typ_list STAR t=typ
    { t::ts }

(***** }}} *****)

(***** Types {{{ *****)

typ:
  | LPAREN t=typ RPAREN
    { t }
  | t1=typ ARR t2=typ
    { TArr (t1, t2) }
  | LPAREN t1=star_typ_list STAR t2=typ RPAREN
    { TTuple(t2 :: List.rev t1)}
  | a=typ TList
    { TList (a) }
  | t=typ_base  
    { t }

typ_base:
  | TBool
    { TBool }
  | TInt
    { TInt }
  | TString
    { TString}
  | d=LID
    { TBase d }

(***** }}} *****)

(***** Args {{{ *****)

arg:
  | LPAREN x=LID COLON t=typ RPAREN
    { (x, t) }
  | x=LID
    { (x,TPoly) }

arg_comma:
  | x=arg
    { [x] }
  | xs=arg_comma COMMA x=arg
    { x :: xs }
  | xs=arg_comma COMMA LPAREN x=arg_comma RPAREN
    { x @ xs }

arg_list:   (* NOTE: reversed *)
  | (* Empty *)
    { [] }
  | xs=arg_list x=arg
    { x :: xs}
  | xs=arg_list LPAREN x=arg_comma RPAREN
    { x @ xs }

(***** }}} *****)

(***** Expressions {{{ *****)

exp:
  | e=exp_app
    { e }
  | e=exp_op
    { e }

exp_app:
  | e=exp_base es=exp_app_list
    { appify e es }
  | e=exp_base
    { e }

exp_app_list:     
  | e=exp_base
    { [e] }
  | e=exp_base es=exp_app_list 
    { e::es }

exp_op:
  | x1=exp PLUS x2=exp
    { ADD(x1,x2) }
  | x1=exp MINUS x2=exp
    { SUB(x1,x2) }
  | x1=exp DIVIDE x2=exp
    { DIV(x1,x2) }
  | x1=exp MOD x2=exp
    { MOD(x1,x2) }
  | x1=exp STAR x2=exp
    { MUL(x1,x2) }
  | NOT x1=exp %prec UNARY
    { NOT(x1) }
  | MINUS x1=exp %prec UNARY
    { MINUS (x1) }
  | x1=exp OR x2=exp
    { OR(x1,x2) }
  | x1=exp AND x2=exp
    { AND(x1,x2) }
  | x1=exp LESS x2=exp
    { LESS(x1,x2) }
  | x1=exp LARGER x2=exp
    { LARGER(x1,x2) }
  | x1=exp LESSEQ x2=exp
    { LESSEQ(x1,x2) }
  | x1=exp LARGEREQ x2=exp
    { LARGEREQ(x1,x2) }
  | x1=exp NOTEQ x2=exp
    { NOTEQ(x1,x2) }
  | x1=exp EQ x2=exp
    { EQUAL(x1,x2) }
  | l1=exp AT l2=exp
    { AT(l1,l2) }
  | l1=exp DOUBLECOLON l2=exp
    { DOUBLECOLON(l1,l2) }

exp_base:
  | c=INT
    { Const c }
  | c=STRING
    { String c }
  | TRUE
    { TRUE }
  | FALSE
    { FALSE }
  | x=LID
    { EVar x }
  | c=UID
    { ECtor (c, []) }
  | c=UID LPAREN es=exp_comma_list RPAREN
    { ECtor (c, es) }
  | LBRACKET l=exp_semi_list RBRACKET
    { list_of_exps l }
  | LPAREN e=exp COMMA l=exp_comma_list RPAREN
    { ETuple (e::l) }
  | FUN xs=arg_list ARR e=exp
    { binding_args (List.rev xs) e }
  | LET f=LID xs=arg_list COLON t=typ EQ e1=exp IN e2=exp %prec ASSIGN
    { ELet (f, false, List.rev xs, t, e1, e2) }
  | LET REC f=LID xs=arg_list COLON t=typ EQ e1=exp IN e2=exp %prec ASSIGN
    { ELet (f, true, List.rev xs, t, e1, e2) }
  | LET f=LID xs=arg_list EQ e1=exp IN e2=exp %prec ASSIGN
    { ELet (f, false, List.rev xs, TPoly, e1, e2) }
  | LET REC f=LID xs=arg_list EQ e1=exp IN e2=exp %prec ASSIGN
    { ELet (f, true, List.rev xs, TPoly, e1, e2) } 
  | MATCH e=exp WITH bs=branches 
    { EMatch (e, List.rev bs) }
  | IF b1=exp THEN x1=exp ELSE x2=exp
    { IF(b1,x1,x2) }
  | LPAREN e=exp RPAREN
    { e }
  | HOLE
    { Lang.gen_hole()}

exp_comma_list:
  | e=exp
    { [e] }
  | e=exp COMMA es=exp_comma_list
    { e :: es }

exp_semi_list:
  | (* empty *)
    { [] }
  | e=exp
    { [e] }
  | e=exp SEMI es=exp_semi_list_one
    { e :: List.rev es }

exp_semi_list_one:    (* NOTE: reversed *)
  | e=exp
    { [e] }
  | es=exp_semi_list_one SEMI e=exp
    { e :: es }

branches:
  |
    { [] }
  | bs=branches b=branch
    { b::bs }

branch:
  | PIPE p=pat ARR e=exp
    { (p, e) }

pat:
  | c=INT
    { PInt (c) }
  | TRUE
    { PBool (true) }
  | FALSE
    { PBool (false) }
  | c=LID
    { PVar (c) }
  | LBRACKET p=pat_semi RBRACKET
    { PList (List.rev p) }
  | LPAREN p1=pat COMMA p2=pat_comma RPAREN
    { PTuple (p1::(List.rev p2))}
  | c=UID
    { PCtor (c, []) }
  | c=UID LPAREN xs=pat_comma RPAREN
    { PCtor (c, xs) }
  | p1=pat DOUBLECOLON p2=pat
    { PCons (p1 :: [p2])}  
  | p1=pat PIPE p2=pat
    { Pats (p1 :: [p2]) }
  | UNDERBAR
    { PUnder }
  | LPAREN p=pat RPAREN
    { p }

pat_semi:
  |
    { [] }
  | p=pat
    { [p] }
  | p1=pat_semi SEMI p2=pat
    { p2::p1 }

pat_comma:
  | p=pat
    { [p] }
  | p1=pat COMMA p2=pat_comma
    { p1::p2 }

(***** }}} *****)