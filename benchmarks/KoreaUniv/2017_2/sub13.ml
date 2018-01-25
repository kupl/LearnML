(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
	match t with
	| Empty -> Empty
	| Node(i, left, right) ->
		Node(i, mirror right, mirror left)	

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec count_nat n =
	match n with
	|ZERO -> 0
	|SUCC i -> 1 + count_nat i  

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	match n2 with
	|ZERO -> n1
	|SUCC i -> SUCC (natadd n1 i)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
	match n2 with
	|ZERO -> ZERO
	|SUCC i -> natadd n1 (natmul n1 i)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	match n2 with
	|ZERO -> SUCC ZERO
	|SUCC i -> natmul n1 (natexp n1 i)

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

let rec apply_env x e =
	match e with
	|[] -> raise (Failure "no value")
	|(y,v)::tl -> if x = y then v else apply_env x tl

let rec check_varlist x vl =
	match vl with
	|[] -> false
	|y::tl -> if x = y then true else check_varlist x tl

let rec eval_f: formula -> (string * bool) list -> bool
= fun f env ->
	match f with
	| True -> true
	| False -> false
	| Var s -> apply_env s env
	| Neg p -> not (eval_f p env)
	| And (p,q) -> (eval_f p env) && (eval_f q env)
	| Or (p,q) -> (eval_f p env) || (eval_f q env)
	| Imply (p,q) -> (not (eval_f p env)) || (eval_f q env)
	| Iff (p,q) -> ((eval_f p env) && (eval_f q env)) || ((not (eval_f p env)) && (not (eval_f q env)))

let rec find_var: formula -> string list -> string list
= fun f vl ->
	match f with
	| True -> vl
	| False -> vl
	| Var s -> if (check_varlist s vl) then vl else s::vl  
	| Neg p -> find_var p vl
	| And (p,q) | Or (p,q) | Imply (p,q) | Iff (p,q) -> find_var p (find_var q vl)

let rec sat_op: formula -> string list -> (string * bool) list -> bool
= fun f varlist env ->
	match varlist with
	|[] -> eval_f f env
	|hd::tl -> let tenv = (hd, true)::env in
			   let fenv = (hd, false)::env in
			   		if (sat_op f tl tenv) then true 
			   		else (sat_op f tl fenv) 

let rec sat : formula -> bool
= fun f ->
	let varlist = find_var f [] in
	let env = [] in
		(sat_op f varlist env)

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec reverse_list
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> (reverse_list tl)@[hd]

let last2head
= fun l -> 
	match (reverse_list l) with
	|[] -> []
	|hd::tl -> hd::(reverse_list tl)

let rec map f l x=
	match l with
	|[] -> []
	|hd::tl -> (f (hd,x))::(map f tl x)

let rec diff_op : aexp * string -> aexp
= fun (e,x) ->
	match e with
	| Const n -> Const 0
	| Var v -> if x = v then Const 1 else Const 0
	| Power (v,n) ->
		if x = v then 
			(match n with
			|0 -> Const 0
			|1 -> Const 1
			|_ -> Times [Const n;Power (v,n-1)])
		else Const 0
	| Times l ->
		(match l with
		|[] -> Const 0
		|hd::tl -> let a = diff_op (hd,x) in
				   let b = diff_op ((Times tl),x) in
				   let l1 = Times (a::tl) in
				   let l2 = Times (hd::b::[]) in
				   Sum (l1::l2::[]))
	| Sum l -> Sum (map diff_op l x)	    

let get_head
= fun l -> 
	match l with
	|[] -> raise (Failure "no head")
	|hd::tl -> hd

let get_tail
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> tl

let rec check_list
= fun l exp ->
	match l with
	|[] -> false
	|hd::tl -> if hd = exp then true
			   else check_list tl exp

let rec clean_timelist
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> match hd with
			   | Const 1 -> tl
			   | Times nl -> clean_timelist nl@tl
			   | _ -> hd::(clean_timelist tl)

let rec clean_sumlist
= fun l ->
	match l with
	|[] -> []
	|hd::tl -> match hd with
			   | Const 0 -> tl
			   | Sum nl -> clean_sumlist nl@tl
			   | _ -> hd::(clean_sumlist tl)

let rec combine_timelist
= fun l n ->
	match l with
	|[] -> (Const n)::[]
	|hd::tl -> match hd with
			   |Const i -> combine_timelist tl (n*i)
			   |_ -> hd::(combine_timelist tl n)

let rec combine_sumlist
= fun l n ->
	match l with
	|[] -> (Const n)::[]
	|hd::tl -> match hd with
			   |Const i -> combine_sumlist tl (n+i)
			   |_ -> hd::(combine_sumlist tl n)

let rec combine
= fun (e,x) ->
	match e with
	| Times l ->
		let combinelist = last2head (combine_timelist l 1) in
			if (get_head combinelist) = Const 1 then Times (map combine (get_tail combinelist) x)
			else Times ((get_head combinelist)::(map combine (get_tail combinelist) x))
	| Sum l ->
		let combinelist = clean_sumlist (combine_sumlist l 0) in
			Sum (map combine combinelist x)
	| _ -> e

let rec cleanup
= fun (e,x) ->
	match e with
	| Const n -> Const n
	| Var x -> Var x
	| Power (x,n) ->
		(match n with
		| 0 -> Const 1
		| 1 -> Var x
		| _ -> Power (x,n))
	| Times l ->
		let cleanlist = clean_timelist l in
			if cleanlist = [] then Const 1
			else if (get_tail cleanlist) = [] then cleanup ((get_head cleanlist),x) 
			else if check_list cleanlist (Const 0) then Const 0
				else Times (map cleanup cleanlist x)
	| Sum l ->
		let cleanlist = clean_sumlist l in
			if cleanlist = [] then Const 0
			else if (get_tail cleanlist) = [] then cleanup ((get_head cleanlist),x) 
			else Sum (map cleanup cleanlist x)

let rec ultraclean
= fun (e,x) ->
	let ce = cleanup (e,x) in
		if ce = e then ce
		else ultraclean (ce,x)

let rec diff : aexp * string -> aexp
= fun (e,x) -> combine(ultraclean ((diff_op (e,x)),x), x)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec eval_e: exp -> int -> int
= fun e env ->
	match e with
	| X -> env
	| INT i -> i
	| ADD (p,q) -> eval_e p env + eval_e q env
	| SUB (p,q) -> eval_e p env - eval_e q env
	| MUL (p,q) -> (eval_e p env)*(eval_e q env)
	| DIV (p,q) -> (eval_e p env)/(eval_e q env)
	| SIGMA (p,q,r) ->
		let a = eval_e p env in
		let b = eval_e q env in
		if a = b then eval_e r a
		else (eval_e r a) + (eval_e (SIGMA (INT (a+1), INT b, r)) env)

let rec calculator : exp -> int
= fun e -> eval_e e 1

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec weigh : branch -> int
= fun b ->
	(match b with
	| SimpleBranch (l,w) -> w
	| CompoundBranch (l,m) -> 
		let (left, right) = m in
			(weigh left)+(weigh right)
	)

let rec balanced : mobile -> bool
= fun m -> 
	let (left, right) = m in
		(match left with
		| SimpleBranch(l1,w1) -> 
			(match right with
			| SimpleBranch(l2,w2) -> if l1*w1 = l2*w2 then true else false
			| CompoundBranch(l2,m2) -> if balanced m2 && l1*w1 = l2*(weigh right) then true else false) 
		| CompoundBranch(l1,m1) -> if balanced m1 then
			(match right with
			| SimpleBranch(l2,w2) -> if l1*(weigh left) = l2*w2 then true else false
			| CompoundBranch(l2,m2) -> if balanced m2 && l1*(weigh left) = l2*(weigh right) then true else false)
			else false
		)

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec badd : bin -> bin -> bin
= fun b1 b2 ->
	match reverse_list b1 with
	|[] -> b2
	|hd1::tl1 -> 
		match reverse_list b2 with
		|[] -> b1
		|hd2::tl2 -> match hd1 with
			|ZERO -> if hd2 = ZERO then (badd (reverse_list tl1) (reverse_list tl2))@[ZERO]
					 else (badd (reverse_list tl1) (reverse_list tl2))@[ONE] 
			|ONE -> if hd2 = ZERO then (badd (reverse_list tl1) (reverse_list tl2))@[ONE]
					else (badd [ONE] (badd (reverse_list tl1) (reverse_list tl2)))@[ZERO]

let rec remove_frontzero
= fun b ->
	match b with
	|[] -> []
	|hd::tl -> match tl with
			   |[] -> hd::[]
			   |_ -> match hd with
			   		 |ZERO -> tl
			   		 |ONE -> b

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
	match reverse_list b2 with
	|[] -> []
	|hd::tl ->
		match hd with
		|ZERO -> remove_frontzero ((bmul b1 (reverse_list tl))@[ZERO]) 
		|ONE -> remove_frontzero (badd ((bmul b1 (reverse_list tl))@[ZERO]) b1)

