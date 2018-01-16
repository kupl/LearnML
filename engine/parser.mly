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
%token EXCEPTION
%token RAISE
%token DEFAND
%token FUNCTION

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
%token STRCON     (* ^ *)
%token IDENT      (* ' *)
%token EOF

%token LISTHD
%token LISTTL
%token LISTMAP
%token LISTMEM
%token LISTEXISTS
%token LISTFILTER
%token LISTAPPEND
%token LISTLENGTH
%token LISTNTH
%token LISTREV
%token LISTFOLDL
%token LISTFOLDR
%token LISTSORT
%token LISTREVMAP
%token LISTMEMQ
%token LISTREVAPD
%token LISTMAPI

%left OR
%left AND
%left LESS LESSEQ LARGER LARGEREQ EQ NOTEQ
%right AT STRCON
%right DOUBLECOLON
%left PLUS MINUS
%left STAR DIVIDE MOD
%right UNARY

%start prog
%type <Lang.examples * Lang.prog> prog
%%

prog:
  | ds=empty_decls EOF
  | ds=decls EOF
    { ([],(List.rev ds)) }
  | ds=decls SEMI SEMI EOF
    { ([],(List.rev ds)) }
  | LBRACE es=examples RBRACE EOF
    { (es, []) }

examples:
  | 
    { [] }
  | e=example es=examples
    { e::es}

example:
  | i=exp_list FATARR o=value SEMI
    { (i,o) }

exp_list:
  | e=exp
    { [e] }
  | e=exp SEMI el=exp_list
    { e::el }

value:
  | v=value_base COMMA vs=value_comma_list
    { VTuple (v::vs) }
  | c=value_base
    { c }

value_base:
  | LPAREN RPAREN
    { VUnit }
  | c=INT
    { VInt c }
  | TRUE
    { VBool true }
  | FALSE
    { VBool false }
  | c=STRING
    { VString c }
  | LBRACKET RBRACKET
    { VList [] }
  | LBRACKET v=value vs=value_semi_list RBRACKET
    { VList (v::vs) }
  | c=UID
    { VCtor (c, []) }
  | c=UID v=value_base
    { VCtor (c, [v]) }
  | MINUS c=INT
    { VInt (-c)}
  | LPAREN v=value RPAREN
    { v }

value_semi_list:
  | 
    { [] }
  | SEMI
    { [] }
  | SEMI v=value vs=value_semi_list
    { v::vs }

value_comma_list : 
  | c=value_base
    { [c] }
  | c=value_base COMMA cl=value_comma_list
    { c::cl }

(***** Declarations {{{ *****)

bind:
  | x=bind_tuple
    { x }

bind_tuple:
  | x=bind_base xs=bind_comma_list
    { BindTuple (x::xs) }
  | x=bind_base
    { x }

bind_comma_list:
  | COMMA x=bind_base 
    { [x] }
  | COMMA x=bind_base xs=bind_comma_list
    { x::xs }

bind_base:
  | x=LID
    { BindOne x }
  | LPAREN x=bind RPAREN
    { x }

empty_decls:
  | 
    { [] }
  | SEMI SEMI 
    { [] }
    
decls:  (* NOTE: reversed *)
  | d=decl
    { [d] }
  | e=exp_bind
    { [e] }
  | ds=decls d=decl
    { d::ds }
  | ds=decls SEMI SEMI d=decl
    { d::ds }
  | ds=decls SEMI SEMI e=exp_bind
    { e::ds }

decl:
  | d=datatype
    { d }
  | l=letbind
    { l }
  | d=except
    { d }

except:
  | EXCEPTION c=ctor
    { DExcept c }

datatype:
  | TYPE d=LID EQ t=typ
    { DEqn (d, t) }
  | TYPE d=LID EQ cs=ctors
    { DData (d, List.rev cs) }
  | DEFAND d=LID EQ t=typ
    { DEqn (d, t) }
  | DEFAND d=LID EQ cs=ctors
    { DData (d,List.rev cs) }

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
  | LET x=bind args=args EQ e=exp (* let f x y = e ;; *)
    { DLet (x, false, args, Type.fresh_tvar (), e) }
  | LET x=bind args=args COLON t=typ EQ e=exp (* let f x y : typ = e ;; *)
    { DLet (x, false, args, t, e) }
  | LET REC x=bind args=args EQ e=exp (* let rec f x y = e ;; *)
    { DLet (x, true, args, Type.fresh_tvar(), e) }
  | LET REC x=bind args=args COLON t=typ EQ e=exp (* let rec f x y : typ = e ;; *)
    { DLet (x, true, args, t, e) }
  | DEFAND x=bind args=args COLON t=typ EQ e=exp
    { DLet (x,true,args,t,e)  }
  | DEFAND x=bind args=args EQ e=exp (* let rec f x y = e ;; *)
    { DLet (x, true, args, Type.fresh_tvar(), e) }

exp_bind:
  | e=exp (* e *)
    { DLet (BindOne "-", false, [], Type.fresh_tvar(), e)}

(***** }}} *****)

(***** Types {{{ *****)

typ:
  | t1=typ_star ARR t2=typ
    { TArr (t1, t2) }
  | t = typ_star
    { t }

typ_star:
  | t=typ_base
    { t }
  | t=typ_base STAR ts=star_typ_list
    { TTuple (t::ts) }

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
  | IDENT s=LID
    { TVar s }

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
  | e=exp_tuple
    { e }

exp_tuple:
  | e=exp_struct es=exp_comma_list
    { ETuple (e::es) }
  | e=exp_struct
    { e }

exp_comma_list:
  | COMMA e=exp_struct
    { [e] }
  | COMMA e=exp_struct es=exp_comma_list
    { e::es }

exp_struct:  
  | MATCH e=exp WITH bs=branches 
    { EMatch (e, bs) }
  | FUN xs=args ARR e=exp_struct
    { binding_args xs e }
  | FUNCTION bs=branches
    { EFun(ArgOne("__fun__",Type.fresh_tvar()),EMatch(EVar "__fun__",bs)) }
  | LET f=bind args=args COLON t=typ EQ e1=exp_struct IN e2=exp_struct
    { ELet (f, false, args, t, e1, e2) }
  | LET REC f=bind args=args COLON t=typ EQ e1=exp_struct IN e2=exp_struct
    { ELet (f, true, args, t, e1, e2) }
  | LET f=bind args=args EQ e1=exp_struct IN e2=exp_struct
    { ELet (f, false, args, Type.fresh_tvar(), e1, e2) }
  | LET REC f=bind args=args EQ e1=exp_struct IN e2=exp_struct
    { ELet (f, true, args, Type.fresh_tvar(), e1, e2) } 
  | IF e1=exp_struct THEN e2=exp_struct ELSE e3=exp_struct
    { IF (e1, e2, e3) }
  | e=exp_op
    { e }
  
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
  | e1=exp_op EQ EQ e2=exp_op
    { EQUAL (e1, e2) }

  | e1=exp_op NOTEQ e2=exp_op
    { NOTEQ (e1, e2) }
  | e1=exp_op AT e2=exp_op
    { AT (e1, e2) }
  | e1=exp_op DOUBLECOLON e2=exp_op
    { DOUBLECOLON (e1, e2) }
  | e1=exp_op STRCON e2 = exp_op
    { STRCON (e1,e2) }
  | RAISE e=exp_base
    { Raise e}
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
  | LISTHD
    { (EVar "__list_hd__") }
  | LISTTL
    { (EVar "__list_tl__") }
  | LISTMAP
    { (EVar "__list_map__") }
  | LISTMEM
    { (EVar "__list_mem__") }
  | LISTEXISTS
    { (EVar "__list_exists__") }
  | LISTFILTER
    { (EVar "__list_filter__") }
  | LISTAPPEND
    { (EVar "__list_append__") }
  | LISTLENGTH
    { (EVar "__list_length__") }
  | LISTNTH
    { (EVar "__list_nth__") }
  | LISTREV
    { (EVar "__list_rev__") }
  | LISTFOLDL
    { (EVar "__list_foldl__") }
  | LISTFOLDR
    { (EVar "__list_foldr__") }
  | LISTSORT
    { (EVar "__list_sort__") }
  | LISTREVMAP
    { (EVar "__list_rev_map__") }
  | LISTMEMQ
    { (EVar "__list_memq__") }
  | LISTREVAPD
    { (EVar "__list_rev_append__") }
  | LISTMAPI
    { (EVar "__list_map_i__") }


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
  | MINUS c=INT
    { PInt (-c) }
  | PLUS c=INT
    { PInt c }
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
