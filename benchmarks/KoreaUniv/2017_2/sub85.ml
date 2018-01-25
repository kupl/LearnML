(*problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree->btree = fun t ->
match t with 
	|Empty -> Empty
	|Node (root, Empty, Empty) -> Node (root, Empty, Empty)
	|Node (root, left, Empty) -> Node (root, Empty, mirror left)
	|Node (root, Empty, right) -> Node (root, mirror right, Empty)
	|Node (root,left,right) -> Node (root, mirror right, mirror left)
;;

(*problem 2*)
type nat = ZERO | SUCC of nat
type oper = ADD | MUL | EXP

let rec p2_subfunc1 : nat-> int-> int = fun n1 int_1 ->
match n1 with 
	|ZERO -> int_1
	|SUCC (nat1)-> p2_subfunc1 nat1 (int_1+1)
let rec expo :int->int->int = fun n p ->
	if p = 0 then 1
	else if p =1 then n
	else n*(expo n (p-1))
let rec p2_subfunc2 : int -> int -> int-> oper -> nat -> nat = fun int_1 int_2 int_3 op nat1 ->
match op with 
	|ADD ->
		if int_3 = (int_1+int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) ADD nat1)
	|MUL ->
		if int_3 = (int_1*int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) MUL nat1)
	|EXP ->
		if int_3 = (expo int_1 int_2) then nat1
		else
		SUCC(p2_subfunc2 int_1 int_2 (int_3 + 1) EXP nat1)
let rec natadd : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 ADD ZERO
let rec natmul : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 MUL ZERO
let rec natexp : nat -> nat -> nat = fun n1 n2 ->
let a = p2_subfunc1 n1 0 in
	let b = p2_subfunc1 n2 0 in 
			p2_subfunc2 a b 0 EXP ZERO
;;

(*problem 3*)

type formula = True | False | Var of string | Neg of formula 
				| And of formula * formula | Or of formula * formula
				| Imply of formula * formula | Iff of formula * formula
type env = (string * formula) list

let update_env : (string * formula)->(string * formula) list->(string * formula) list = fun (a,b) env->
	(a,b)::env;;

let rec find_env : string->(string * formula) list->formula = fun s env->
	match env with 
	|[]->raise(Failure "error")
	|(x,en)::tl->(if s=x then en
				else find_env s tl
			)
;;

let rec overlap : string list -> string -> formula = fun lst s ->
match lst with 
	|[]->False
	|hd::tl->if hd = s then True
			else overlap tl s
;;
let rec varlst : formula -> string list ->string list= fun f lst->
match f with
	|True -> lst
	|False -> lst
	|Var x-> if (overlap lst x) = False then x::lst
			else lst
	|Neg a->varlst a lst
	|And (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Or (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Imply (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
	|Iff (a,b)->(if (varlst a lst = varlst b lst) then varlst a lst
				else varlst a lst@varlst b lst)
;;

let rec length : 'a list -> int = fun lst->
match lst with |[]->0 |hd::tl->1+(length tl);;

let rec length_plus : string list->formula list->formula list = fun slst flst->
if length slst != length flst then ([False]@flst)
else flst
;;

let rec dec_to_formula : int->formula list = fun n->
if (n/2 != 1 && n/2 !=0 && n mod 2 = 0) then dec_to_formula(n/2)@[False]
else if (n/2 != 1 && n/2 !=0 && n mod 2 = 1) then dec_to_formula(n/2)@[True]
else if (n/2 = 1 && n mod 2 = 0) then [True;False]
else if (n/2 = 1 && n mod 2 = 1) then [True;True]
else if n = 1 then [True]
else [False]
;;

let rec mkenv : string list -> formula list ->(string*formula) list  = fun lst flst->
	match lst with 
	|hd::tl->(match flst with
				|hd2::tl2->(hd,hd2)::mkenv tl tl2
				|[]->(hd,False)::mkenv tl []
			)
	|[]->[]
;;
let rec sat2 : formula -> env-> formula = fun f env->
match f with
	|True -> True
	|False -> False
	|Var x-> find_env x env 
	|Neg a -> if (sat2 a env)=True then False
				else True
	|And (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->False
						|(False,False)->False
					)
	|Or (a,b) -> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->True
						|(False,True)->True
						|(False,False)->False
					)
	|Imply (a,b)-> let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->True
						|(False,False)->True
					)
	|Iff (a,b)->let (c,d) = (sat2 a env,sat2 b env) in
					(match (c,d) with 
						|(True,True)->True
						|(True,False)->False
						|(False,True)->False
						|(False,False)->True
					)
;;
let rec sat3 : string list->formula ->int ->formula list->formula list = fun slst f n flst->
let a = length slst in
	let b = (expo 2 a) in
		(if(n!=b) then
			[(sat2 f (mkenv slst (length_plus slst (dec_to_formula n))))]@
			(sat3 slst f (n+1) flst)
		else 
		[]
		)
;;
let rec find_true : formula list->bool = fun flst->
match flst with 
	|[]->false
	|hd::tl->(if hd=True then true
			else find_true tl
	)
;;
let rec sat : formula -> bool = fun f->
	let a = (varlst f []) in
		let b= sat3 a f 0 [] in
		find_true b
;;

(*problem 4*)
type aexp = | Const of int | Var of string | Power of string*int
			| Times of aexp list | Sum of aexp list

let rec diff : aexp * string -> aexp = fun (e,x)->
match e with 
	|Const a -> Const 0
	|Var a -> if (x=a) then Const 1 
				else Const 0
	|Power (a,n) -> if a = x then 
						(if n>=2 then Times [Const n;Power (x,n-1)]
						else if n=1 then Const 1
						else Const 0
						)
					else Const 0 
	|Times lst -> (match lst with 
					|[e1;e2]->Sum [Times [diff (e1,x);e2];Times [e1;diff (e2,x)]]
					|hd::tl->Sum [Times [diff (hd,x);Times tl];Times [hd;diff (Times tl,x)]]
				)
	|Sum lst -> (match lst with
					|[e1;e2]->Sum [diff (e1,x);diff (e2,x)]
					|hd::tl->Sum [diff (hd,x);diff (Sum tl,x)]
	)
(*problem 5*)
type exp =X |INT of int |ADD of exp*exp |SUB of exp*exp
			 |MUL of exp*exp	|DIV of exp*exp |SIGMA of exp*exp*exp 

let rec sigma : exp->int->int = fun exp n ->
	match exp with
			|X -> n
			|INT a -> a
			|ADD (a,b) -> (sigma a n)+(sigma b n)
			|SUB (a,b) -> (sigma a n)-(sigma b n)
			|MUL (a,b) -> (sigma a n)*(sigma b n)
			|DIV (a,b) -> (sigma a n)/(sigma b n)
;;

let rec calculator : exp->int = fun e->
	match e with 
	|INT a -> a
	|ADD (a,b) -> (calculator a)+(calculator b)
	|SUB (a,b) -> (calculator a)-(calculator b)
	|MUL (a,b) -> (calculator a)*(calculator b)
	|DIV (a,b) -> (calculator a)/(calculator b)
	|SIGMA (exp1,exp2,exp3) ->
	let int1 = calculator exp1 in(
		let int2 = calculator exp2 in(
		if int1=int2 then sigma exp3 int1
		else (sigma exp3 int1) + (calculator (SIGMA(INT (int1+1),INT int2, exp3)))
		)
	)
;;

(*problem 6*)
type mobile = branch * branch
and branch = SimpleBranch of length * weight
			| CompoundBranch of length * mobile
and length = int
and weight = int

let rec sum_weight : mobile->int = fun b->
match b with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) -> b+d
	|(SimpleBranch(a,b),CompoundBranch(c,d)) -> b+(sum_weight d)
	|(CompoundBranch(a,b),SimpleBranch(c,d)) -> d+(sum_weight b)
	|(CompoundBranch(a,b),CompoundBranch(c,d)) -> (sum_weight b)+(sum_weight d)
;;
let rec balanced : mobile->bool = fun m->
match m with
	|(SimpleBranch(a,b),SimpleBranch(c,d)) ->
		if (a*b)=(c*d) then true
		else false 
	|(CompoundBranch(a,b),SimpleBranch(c,d)) ->
		if ((balanced b) = true && (sum_weight b)*a=d*c) then true
		else false 
	|(SimpleBranch(a,b),CompoundBranch(c,d)) ->
		if ((balanced d) = true && (sum_weight d)*c=b*a) then true
		else false 
	|(CompoundBranch(a,b),CompoundBranch(c,d)) ->
		if ((balanced b) = true && (balanced d) = true && (sum_weight b)*a=(sum_weight d)*c)
			then true
		else false
;;

(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec list_length : bin->int = fun b1->
	match b1 with
		|[]->0 
		|hd::tl->(list_length tl)+1
;;

let rec bin_to_dec : bin->int = fun b1 ->
let leng = list_length b1 in
	match b1 with
		|[]->0
		|hd::tl->if hd = ONE then (expo 2 (leng-1)) + bin_to_dec tl
				else bin_to_dec tl
;;

let rec dec_to_bin : int->bin = fun n->
if (n/2 != 1 && n/2 !=0 && n mod 2 = 0) then dec_to_bin(n/2)@[ZERO]
else if (n/2 != 1 && n/2 !=0 && n mod 2 = 1) then dec_to_bin(n/2)@[ONE]
else if (n/2 = 1 && n mod 2 = 0) then [ONE;ZERO]
else if (n/2 = 1 && n mod 2 = 1) then [ONE;ONE]
else if n = 1 then [ONE]
else [ZERO]
;;

let rec bmul : bin->bin->bin = fun b1 b2 ->
let a = bin_to_dec b1 in
	let b = bin_to_dec b2 in
		dec_to_bin(a*b)
;;
let a = And(Var "P", Neg (Var "Q"));;
let b = And(Var "P", Neg (Var "P"));;
let c = And(True,False);;
let d = Or(And(True,False),Or(True,False));;
let e = Or(And(Var "P", Var"Q"), Or(Var "P", Var "Q"));;
let f = Imply(Var "P",Var "Q");;
let g = Imply(And(Var "P",Var "Q"),Or(False,False));;
let h = Iff(Var "P",Var "Q");;
let i = Iff(And (Var "P",Var "Q"), Or (Var "P",Var "Q"));;
