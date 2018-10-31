%{
open Lang

exception Internal_error of string

let rec appify (e:lexp) (es:lexp list) : lexp =
  match es with
  | [] -> e
  | e'::es -> appify (gen_label(),EApp (e, e')) es

let rec binding_args : arg list -> lexp -> lexp
= fun args e ->
  match args with
  | [] -> e
  | hd::tl -> (gen_label(),EFun (hd, binding_args tl e))
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
%token LISTFORALL
%token LISTFIND
%token LISTASSOC
%token STRINGCONCAT

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

(***** Declarations {{{ *****)

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
  | d=datatype_block
    { d }
  | l=letbind_block
    { l }
  | d=except
    { d }

except:
  | EXCEPTION c=ctor
    { DExcept c }

datatype_block:
  | d=datatype ds=datatype_and_list
    { TBlock (d::ds) }
  | d=datatype
    { d }

datatype:
  | TYPE d=datatype_bind
    { d }

datatype_and_list:
  | DEFAND d=datatype_bind
    { [d] }
  | DEFAND d=datatype_bind ds=datatype_and_list
    { d::ds }

datatype_bind:
  | d=LID EQ t=typ
    { DEqn (d, t) }
  | d=LID EQ cs=ctors
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

letbind_block:
  | LET d=letbind ds=let_and_list
    { DBlock (false, d::ds) }
  | LET REC d=letrec_bind ds=letrec_and_list
    { DBlock (true, d::ds) }
  | LET d=letbind | LET REC d=letrec_bind
    { DLet d }

let_and_list:
  | DEFAND x=bind args=args COLON t=typ EQ e=exp
    { [(x, false, args,t,e)] }
  | DEFAND x=bind args=args EQ e=exp 
    { [(x, false, args, fresh_tvar(), e)] }
  | DEFAND x=bind args=args COLON t=typ EQ e=exp ds=let_and_list
    { (x, false, args, t, e)::ds }
  | DEFAND x=bind args=args EQ e=exp ds=let_and_list
    { (x, false, args, fresh_tvar(), e)::ds }

letrec_and_list:
  | DEFAND x=bind args=args COLON t=typ EQ e=exp
    { [(x, true, args,t,e)] }
  | DEFAND x=bind args=args EQ e=exp 
    { [(x, true, args, fresh_tvar(), e)] }
  | DEFAND x=bind args=args COLON t=typ EQ e=exp ds=letrec_and_list
    { (x, true, args,t,e)::ds }
  | DEFAND x=bind args=args EQ e=exp ds=letrec_and_list
    { (x, true, args, fresh_tvar(), e)::ds }

exp_bind:
  | e=exp (* e *)
    { DLet (BindUnder, false, [], fresh_tvar(), e)}

(***** }}} *****)

(***** Binding {{{ *****)

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
  | UNDERBAR
    { BindUnder }
  | x=LID
    { BindOne x }
  | LPAREN x=bind RPAREN
    { x }

letbind:
  | x=bind args=args EQ e=exp (* let f x y = e ;; *)
    { (x, false, args, fresh_tvar (), e) }
  | x=bind args=args COLON t=typ EQ e=exp (* let f x y : typ = e ;; *)
    { (x, false, args, t, e) }

letrec_bind:
  | x=bind args=args EQ e=exp (* let f x y = e ;; *)
    { (x, true, args, fresh_tvar (), e) }
  | x=bind args=args COLON t=typ EQ e=exp (* let f x y : typ = e ;; *)
    { (x, true, args, t, e) }

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
  | UNDERBAR
    { ArgUnder (fresh_tvar ()) }
  | LPAREN UNDERBAR COLON t=typ RPAREN
    { ArgUnder t }
  | x=LID
    { ArgOne (x, fresh_tvar ()) }
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
    { (gen_label(),ETuple (e::es)) }
  | e=exp_struct
    { e }

exp_comma_list:
  | COMMA e=exp_struct
    { [e] }
  | COMMA e=exp_struct es=exp_comma_list
    { e::es }

exp_struct:  
  | MATCH e=exp WITH bs=branches 
    { (gen_label(),EMatch (e, bs)) }
  | FUN xs=args ARR e=exp
    { binding_args xs e }
  | FUNCTION bs=branches
    { (gen_label(), EFun(ArgOne("__fun__",fresh_tvar()),(gen_label(),EMatch((gen_label(),EVar "__fun__"),bs)))) }
  | LET f=bind args=args COLON t=typ EQ e1=exp IN e2=exp
    { (gen_label(), ELet (f, false, args, t, e1, e2)) }
  | LET REC f=bind args=args COLON t=typ EQ e1=exp IN e2=exp
    { (gen_label(), ELet (f, true, args, t, e1, e2)) }
  | LET f=bind args=args EQ e1=exp IN e2=exp
    { (gen_label(), ELet (f, false, args, fresh_tvar(), e1, e2)) }
  | LET REC f=bind args=args EQ e1=exp IN e2=exp
    { (gen_label(), ELet (f, true, args, fresh_tvar(), e1, e2) )} 
  | LET d=letbind ds=let_and_list IN e2=exp
    { (gen_label(), EBlock (false, d::ds, e2)) }
  | LET REC d=letrec_bind ds=letrec_and_list IN e2 = exp
    { (gen_label(), EBlock (true, d::ds, e2)) }
  | IF e1=exp_struct THEN e2=exp_struct ELSE e3=exp_struct
    { (gen_label(), IF (e1, e2, e3)) }
  | e=exp_op
    { e }
  
exp_op:
  | e1=exp_op PLUS e2=exp_op
    { (gen_label(), ADD (e1, e2)) }
  | e1=exp_op MINUS e2=exp_op
    { (gen_label(), SUB (e1, e2)) }
  | e1=exp_op STAR e2=exp_op
    { (gen_label(), MUL (e1, e2)) }
  | e1=exp_op DIVIDE e2=exp_op
    { (gen_label(), DIV (e1, e2)) }
  | e1=exp_op MOD e2=exp_op
    { (gen_label(), MOD (e1, e2)) }
  | NOT e=exp_op %prec UNARY
    { (gen_label(), NOT e) }
  | MINUS e=exp_op %prec UNARY
    { (gen_label(), MINUS e) }
  | e1=exp_op OR e2=exp_op
    { (gen_label(), OR (e1, e2)) }
  | e1=exp_op AND e2=exp_op
    { (gen_label(), AND (e1, e2)) }
  | e1=exp_op LESS e2=exp_op
    { (gen_label(), LESS (e1, e2)) }
  | e1=exp_op LARGER e2=exp_op
    { (gen_label(), LARGER (e1, e2)) }
  | e1=exp_op LESSEQ e2=exp_op
    { (gen_label(), LESSEQ (e1, e2)) }
  | e1=exp_op LARGEREQ e2=exp_op
    { (gen_label(), LARGEREQ (e1, e2)) }
  | e1=exp_op EQ e2=exp_op
    { (gen_label(), EQUAL (e1, e2)) }
  | e1=exp_op EQ EQ e2=exp_op
    { (gen_label(), EQUAL (e1, e2)) }
  | e1=exp_op NOTEQ e2=exp_op
    { (gen_label(), NOTEQ (e1, e2)) }
  | e1=exp_op AT e2=exp_op
    { (gen_label(), AT (e1, e2)) }
  | e1=exp_op DOUBLECOLON e2=exp_op
    { (gen_label(), DOUBLECOLON (e1, e2)) }
  | e1=exp_op STRCON e2 = exp_op
    { (gen_label(), STRCON (e1,e2)) }
  | RAISE e=exp_base
    { (gen_label(), Raise e) }
  | e=exp_base es=exp_app_list (* funcion call or constant *)
    { appify e es }
  | c=UID e=exp_base
    { (gen_label(), ECtor (c, [e])) }

exp_app_list:     
  | 
    { [] }
  | e=exp_base es=exp_app_list 
    { e::es }
  
exp_base:
  | c=INT
    { (gen_label(), Const c) }
  | x=LID
    { (gen_label(), EVar x) }
  | c=STRING
    { (gen_label(), String c) }
  | TRUE
    { (gen_label(), TRUE) }
  | FALSE
    { (gen_label(), FALSE) }
  | LPAREN RPAREN
    { (gen_label(), EUnit) }
  | LBRACKET RBRACKET
    { (gen_label(), EList []) }
  | LBRACKET e=exp es=exp_semi_list RBRACKET
    { (gen_label(), EList (e::es)) }
  | c=UID
    { (gen_label(), ECtor (c, [])) }
  | LPAREN e=exp RPAREN
    { e }
  | BEGIN e=exp END
    { e }
  | HOLE
    { (gen_label(), Lang.gen_hole()) }
  | LISTHD
    { (gen_label(), (EVar "__list_hd__")) }
  | LISTTL
    { (gen_label(), (EVar "__list_tl__")) }
  | LISTMAP
    { (gen_label(), (EVar "__list_map__")) }
  | LISTMEM
    { (gen_label(), (EVar "__list_mem__")) }
  | LISTEXISTS
    { (gen_label(), (EVar "__list_exists__")) }
  | LISTFILTER
    { (gen_label(), (EVar "__list_filter__")) }
  | LISTAPPEND
    { (gen_label(), (EVar "__list_append__")) }
  | LISTLENGTH
    { (gen_label(), (EVar "__list_length__")) }
  | LISTNTH
    { (gen_label(), (EVar "__list_nth__")) }
  | LISTREV
    { (gen_label(), (EVar "__list_rev__")) }
  | LISTFOLDL
    { (gen_label(), (EVar "__list_foldl__")) }
  | LISTFOLDR
    { (gen_label(), (EVar "__list_foldr__")) }
  | LISTSORT
    { (gen_label(), (EVar "__list_sort__")) }
  | LISTREVMAP
    { (gen_label(), (EVar "__list_rev_map__")) }
  | LISTMEMQ
    { (gen_label(), (EVar "__list_memq__")) }
  | LISTREVAPD
    { (gen_label(), (EVar "__list_rev_append__")) }
  | LISTMAPI
    { (gen_label(), (EVar "__list_map_i__")) }
  | LISTFORALL
    { (gen_label(), (EVar "__list_for_all__")) }
  | LISTFIND
    { (gen_label(), (EVar "__list_find__")) }
  | LISTASSOC
    { (gen_label(), (EVar "__list_assoc__")) }
  | STRINGCONCAT
    { (gen_label(), EVar "__string_concat__") }

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

(***** Testcase {{{ *****)

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

(***** }}} *****)
