%{
open Lang

exception Internal_error of string

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
%token TUnit
%token BEGIN
%token END

%token HOLE       (* ? *)
(* %token IMPLIES    (* |> *) *)
%token EQ         (* = *)
%token ARR        (* -> *)
%token FATARR     (* => *)
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

%left OR
%left AND
%left LESS LESSEQ LARGER LARGEREQ EQ NOTEQ
%right AT
%right DOUBLECOLON
%left PLUS MINUS
%left STAR DIVIDE MOD
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
  | 
    { [] }
  | e=example es=examples
    { e::es}

example:
  | i=exp_list FATARR o=value SEMI
    { (i,o) }

exp_list:
  | e=exp_op
    { [e] }
  | e=exp_op COMMA el=exp_list
    { e::el }

value:
  | c=value_base
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
    { c:: cl }

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

ctors:
  | c=ctor
    { [c] }
  | PIPE c=ctor
    { [c] }
  | cs=ctors PIPE c=ctor
    { cs@[c] }

ctor:
  | c=UID
    { (c, []) }
  | c=UID OF t=typ
    { (c, [t]) }

letbind:
  | LET x=LID args=args EQ e=exp SEMI SEMI (* let f x y = e ;; *)
    { DLet (x, false, args, Type.fresh_tvar (), e) }
  | LET x=LID args=args COLON t=typ EQ e=exp SEMI SEMI (* let f x y : typ = e ;; *)
    { DLet (x, false, args, t, e) }
  | LET REC x=LID args=args EQ e=exp SEMI SEMI (* let rec f x y = e ;; *)
    { DLet (x, true, args, Type.fresh_tvar(), e) }
  | LET REC x=LID args=args COLON t=typ EQ e=exp SEMI SEMI (* let rec f x y : typ = e ;; *)
    { DLet (x, true, args, t, e) }
  | e=exp SEMI SEMI (* e *)
    { DLet ("@", false, [], Type.fresh_tvar(), e)}

(***** }}} *****)

(***** Types {{{ *****)

typ:
  | t1=typ_base ARR t2=typ
    { TArr (t1, t2) }
  | t=typ_base STAR ts=star_typ_list
    { TTuple (t::ts) }
  | t = typ_base
    { t }

star_typ_list:  (* NOTE: reversed *)
  | t=typ_base
    { [t] }
  | t=typ_base STAR ts=star_typ_list
    { t::ts }
  
typ_base:
  | TInt
    { TInt }
  | TString
    { TString}
  | TUnit
    { TUnit }
  | d=LID
    { TBase d }
  | t=typ_base TList
    { TList t }
  | TBool
    { TBool }
  | LPAREN t=typ RPAREN
    { t }

(***** }}} *****)

(***** Args {{{ *****)

args:  
  | 
    { [] }
  | x=arg xs=args
    { x :: xs}

arg:
  | x=LID
    { ArgOne (x, Type.fresh_tvar ()) }
  | LPAREN x=LID COLON t=typ RPAREN
    { ArgOne (x, t) }
  | LPAREN x=arg COMMA xs=arg_comma_list RPAREN
    { ArgTuple (x::xs) }
  | LPAREN x=arg RPAREN
    { x }

arg_comma_list:
  | x=arg
    { [x] }
  | x=arg COMMA xs=arg_comma_list
    { x::xs }

(***** }}} *****)

(***** Expressions {{{ *****)

exp:
  | MATCH e=exp WITH bs=branches 
    { EMatch (e, bs) }
  | FUN xs=args ARR e=exp
    { binding_args (List.rev xs) e }
  | LET f=LID args=args COLON t=typ EQ e1=exp IN e2=exp
    { ELet (f, false, args, t, e1, e2) }
  | LET REC f=LID args=args COLON t=typ EQ e1=exp IN e2=exp
    { ELet (f, true, args, t, e1, e2) }
  | LET f=LID args=args EQ e1=exp IN e2=exp
    { ELet (f, false, args, Type.fresh_tvar(), e1, e2) }
  | LET REC f=LID args=args EQ e1=exp IN e2=exp
    { ELet (f, true, args, Type.fresh_tvar(), e1, e2) } 
  | IF e1=exp THEN e2=exp ELSE e3=exp
    { IF (e1, e2, e3) }
  | e=exp_tuple
    { e }

exp_tuple:
  | e=exp_op es=exp_comma_list
    { ETuple (e::es) }
  | e=exp_op
    { e }

exp_comma_list:
  | COMMA e=exp_op
    { [e] }
  | COMMA e=exp_op es=exp_comma_list
    { e::es }

exp_op:
  | e1=exp_op PLUS e2=exp_op
    { ADD (e1, e2) }
  | e1=exp_op MINUS e2=exp_op
    { SUB (e1, e2) }
  | e1=exp_op STAR e2=exp_op
    { MUL (e1, e2) }
  | e1=exp_op DIVIDE e2=exp_op
    { DIV (e1, e2) }
  | e1=exp_op MOD e2=exp_op
    { MOD (e1, e2) }
  | NOT e=exp_op %prec UNARY
    { NOT e }
  | MINUS e=exp_op %prec UNARY
    { MINUS e }
  | e1=exp_op OR e2=exp_op
    { OR (e1, e2) }
  | e1=exp_op AND e2=exp_op
    { AND (e1, e2) }
  | e1=exp_op LESS e2=exp_op
    { LESS (e1, e2) }
  | e1=exp_op LARGER e2=exp_op
    { LARGER (e1, e2) }
  | e1=exp_op LESSEQ e2=exp_op
    { LESSEQ (e1, e2) }
  | e1=exp_op LARGEREQ e2=exp_op
    { LARGEREQ (e1, e2) }
  | e1=exp_op EQ e2=exp_op
    { EQUAL (e1, e2) }
  | e1=exp_op NOTEQ e2=exp_op
    { NOTEQ (e1, e2) }
  | e1=exp_op AT e2=exp_op
    { AT (e1, e2) }
  | e1=exp_op DOUBLECOLON e2=exp_op
    { DOUBLECOLON (e1, e2) }
  | e=exp_base es=exp_app_list (* funcion call or constant *)
    { appify e es }
  | c=UID e=exp_base
    { ECtor (c, [e]) }

exp_app_list:     
  | 
    { [] }
  | e=exp_base es=exp_app_list 
    { e::es }
  
exp_base:
  | c=INT
    { Const c }
  | x=LID
    { EVar x }
  | c=STRING
    { String c }
  | TRUE
    { TRUE }
  | FALSE
    { FALSE }
  | LPAREN RPAREN
    { EUnit }
  | LBRACKET RBRACKET
    { EList [] }
  | LBRACKET e=exp es=exp_semi_list RBRACKET
    { EList (e::es) }
  | c=UID
    { ECtor (c, []) }
  | LPAREN e=exp RPAREN
    { e }
  | BEGIN e=exp END
    { e }
  | HOLE
    { Lang.gen_hole() }

exp_semi_list:
  |
    { [] }
  | SEMI
    { [] }
  | SEMI e=exp es=exp_semi_list
    { e::es }
  

(***** }}} *****)

(***** Branch & Pattern {{{ *****)

branches:
  | b=branch
    { [b] }
  | PIPE b=branch
    { [b] }
  | bs=branches PIPE b=branch 
    { bs@[b] }

branch:
  | p=pat ARR e=exp
    { (p, e) }

pat:
  | p=pat_tuple PIPE ps=pat_list
    { Pats (p :: ps) }
  | p=pat_tuple
    { p }

pat_list:
  | p=pat_tuple
    { [p] }
  | p=pat_tuple PIPE ps=pat_list
    { p::ps }


pat_tuple:
  | p=pat_op
    { p }
  | p=pat_op COMMA ps=pat_comma_list
    { PTuple (p::ps)}

pat_comma_list:
  | p=pat_op
    { [p] }
  | p=pat_op COMMA ps=pat_comma_list
    { p::ps }

pat_op:
  | p1=pat_op DOUBLECOLON p2=pat_op
    { PCons (p1::[p2]) }
  | c=UID ps=pat_base
    { PCtor (c, [ps]) }
  | p=pat_base
    { p }

pat_base:
  | LPAREN RPAREN
    { PUnit }
  | c=INT
    { PInt (c) }
  | TRUE
    { PBool true }
  | FALSE
    { PBool false }
  | x=LID
    { PVar x }
  | UNDERBAR
    { PUnder }
  | c=UID
    { PCtor (c, []) }
  | LBRACKET RBRACKET
    { PList [] }
  | LBRACKET p=pat ps=pat_semi_list RBRACKET
    { PList (p::ps) }
  | LPAREN p=pat RPAREN
    { p }

pat_semi_list:
  |
    { [] }
  | SEMI 
    { [] }
  | SEMI p=pat ps=pat_semi_list
    { p::ps }

(***** }}} *****)