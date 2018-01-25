(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  |Empty->t
  |Node(x,y,z)->Node(x, mirror z, mirror y)


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  |ZERO->n1
  |SUCC n->natadd (SUCC n1) n

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  |ZERO->n2
  |SUCC n->natadd (natmul n1 n) n1

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  |ZERO->(SUCC ZERO)
  |SUCC n->natmul n1 (natexp n1 n)


(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec make_env x l= match x with
  |True|False->l
  |Var str->if List.exists (fun x->if x=str then true else false) l then l else str::l
  |Neg p->make_env p l
  |And (p,q)|Or (p,q)|Imply (p,q)|Iff (p,q)->make_env q (make_env p l)

let rec make_table lst l cnt = match l with
  |[]->lst
  |hd::tl->if cnt mod 2 = 1 then make_table ((hd,True)::lst) tl (cnt/2)
    else make_table ((hd,False)::lst) tl (cnt/2)

let rec interpret f l= match f with
  |Var s->let (x,v)=List.find (fun (y,z)->if y=s then true else false) l in v
  |True|False->f
  |Neg p->Neg (interpret p l)
  |And (p,q)->And ((interpret p l),(interpret q l))
  |Or (p,q)->Or ((interpret p l),(interpret q l))
  |Imply (p,q)->Imply ((interpret p l),(interpret q l))
  |Iff (p,q)->Iff ((interpret p l),(interpret q l))

let rec alu f = match f with
   True->true
  |False->false
  |Neg p->if alu p=false then true else false
  |And (p,q)->if alu p=true && alu q=true then true else false
  |Or (p,q)->if alu p=false &&alu q=false then false else true
  |Imply (p,q)->if alu p=false then true else alu q
  |Iff (p,q)->if alu p=alu q then true else false


let rec sat : formula -> bool
= fun f ->
  let varlist = make_env f [] in
  let length=int_of_float (2.0 ** (float_of_int (List.length varlist))) in
  let rec cnt_sat i = if alu (interpret f (make_table [] varlist i))=true then true else
    if i+1=length then false
    else cnt_sat (i+1) in cnt_sat 0


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec simplifier f = let res = begin match f with
  |Times l->if List.exists (fun x->if x=Const 0 then true else false) l then Const 0
						else begin match l with
							|[]->Times []
							|hd::tl->let lst = begin match simplifier (Times tl) with
												|Times lst->lst
												|_->[] end in Times ((simplifier hd)::lst) end
  |Sum l->begin match l with
    |[]->Sum []
    |hd::tl->if hd=Const 0 then Sum tl
						 else let lst = begin match simplifier (Sum tl) with
                   |Sum lst->lst
                   |_->[] end in Sum ((simplifier hd)::lst) end
  |Power (s,n)->begin match n with
		|0->Const 1
		|1->Var s
		|_->f end
  |_->f end in if f=res then res else simplifier res


let rec diff : aexp * string -> aexp
= fun (e,x) -> let res = begin match e with
  |Const n->Const 0
  |Var s->if s=x then Const 1 else Const 0
  |Times l->begin match l with
    |[]->Const 0
    |hd::tl->Sum [Times ((diff (hd,x))::tl); Times [hd; (diff (Times tl, x))]] end
  |Sum l->begin match l with
		|[]->Const 0
    |[y]->diff (y,x)
    |hd::tl->let l1=[diff (hd,x)] in 
      let l2 = begin match diff (Sum tl,x) with
       |Sum l->l1@l
       |i->l1@[i] end
      in Sum l2 end
  |Power (s,n)->begin match (s,n) with
    |(_,0)->Const 0
    |(s,1)->diff (Var s,x)
    |(p,q)->if p=x then Times [Const q;Power (p, q-1)] else Const 0 end
  end in simplifier res

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec intprt f i= match f with
  |X->INT i
  |INT j->INT j
  |ADD (p,q)->ADD (intprt p i, intprt q i)
  |SUB (p,q)->SUB (intprt p i, intprt q i)
  |MUL (p,q)->MUL (intprt p i, intprt q i)
  |DIV (p,q)->DIV (intprt p i, intprt q i)

let rec calculator : exp -> int
= fun e ->match e with
  |INT i->i
  |ADD (p,q)->calculator p + calculator q
  |SUB (p,q)->calculator p - calculator q
  |MUL (p,q)->(calculator p)*(calculator q)
  |DIV (p,q)->(calculator p)/(calculator q)
  |SIGMA (i,j,p)->if calculator i>calculator j then 0
           else calculator (ADD(intprt p (calculator i),SIGMA (ADD (i,INT 1),j,p)))


(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m ->
 let rec weightsum x=match x with
  |SimpleBranch (l,w)->w
  |CompoundBranch (l,n)->let (b,c)=n in weightsum b + weightsum c
 in let rec mobilesum n= let (b,c)=n in weightsum b + weightsum c
 in let torquesum x=match x with
  |SimpleBranch (l,w)->l*w
  |CompoundBranch (l,n)->l*mobilesum n
 in let det x=match x with
  |SimpleBranch (y,z)->true
  |CompoundBranch (y,z)->balanced z
 in let (p,q)=m in if torquesum p=torquesum q then det p && det q else false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec baddd b1 b2 c = match (b1,b2) with
  |([],[])->if c=ZERO then [] else [ONE]
  |([],l)|(l,[])->if c=ZERO then l else baddd [c] l ZERO
  |(l1,l2)->let t1=(List.length l1-1) in let t2=(List.length l2-1) in
  let new_c = match (List.nth b1 t1, List.nth b2 t2, c) with
    |(ONE,ONE,ONE)|(ONE,ONE,ZERO)|(ONE,ZERO,ONE)|(ZERO,ONE,ONE)->ONE
    |_->ZERO
  in let s = match (List.nth b1 t1, List.nth b2 t2, c) with
    |(ONE,ZERO,ZERO)|(ZERO,ONE,ZERO)|(ZERO,ZERO,ONE)|(ONE,ONE,ONE)->ONE
    |_->ZERO
  in let cutter b = List.rev (List.tl (List.rev b))
  in (baddd (cutter b1) (cutter b2) new_c)@[s]

let rec badd b1 b2 = baddd b1 b2 ZERO

let rec bshl b n = match n with
  |0->b
  |_->bshl (b@[ZERO]) (n-1)

let rec cntmul x y res i = if i=(-1) then res
  else match x with
  |[]->res
  |hd::tl->if hd=ONE then (cntmul tl y (badd res (bshl y i)) (i-1)) else (cntmul tl y res (i-1))

let bmul : bin -> bin -> bin
= fun b1 b2 -> cntmul b1 b2 [ZERO] ((List.length b1) -1)
