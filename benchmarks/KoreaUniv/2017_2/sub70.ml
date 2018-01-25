(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
match t with
| Empty -> Empty
| Node (a,b,c) -> Node (a,(mirror c),(mirror b))

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> n2
| SUCC a -> natadd a (SUCC n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec natmul_tmp : nat->nat->nat->nat
	= fun n1_origin n1 n2->
		match n2 with
		|ZERO -> ZERO
		|SUCC ZERO -> n1
		|SUCC a -> natmul_tmp n1_origin (natadd n1_origin n1) a
	in
natmul_tmp n1 n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec natexp_tmp : nat->nat->nat->nat
	= fun n1_origin n1 n2->
		match n2 with
		|ZERO -> SUCC ZERO
		|SUCC ZERO -> n1
		|SUCC a -> natexp_tmp n1_origin (natmul n1_origin n1) a
	in
natexp_tmp n1 n1 n2

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

let rec sat : formula -> bool
= fun f -> 
(*Imply(a,b) = Or(Neg(a),b)*)
(*Iff(a,b) = Or(And(a,b),And(Neg(a),Neg(b))) *)
	let compare_Var : formula -> formula -> bool
	= fun a b ->
		match a,b with
		| Var(k), Var(m) -> (compare k m)==0
		| _,_ -> false
	in

	let rec exist_Var : formula -> formula list -> bool
	= fun a l ->
		match l with
		| [] -> false
		| [x] -> (compare_Var a x)
		| hd::tl -> ((compare_Var a hd)||(exist_Var a tl))
	in
	
	let rec search_Var : formula -> formula list -> formula list
	= fun a l->
		match a with
		| True -> l
		| False -> l
		| Var(x) -> if (exist_Var (Var(x)) l) then l else (Var(x)::l)
		| Neg(x) -> (search_Var x l)
		| And(x,y) -> (search_Var x l)@(search_Var y l)
		| Or(x,y) -> (search_Var x l)@(search_Var y l)
		| Imply(x,y) -> (search_Var x l)@(search_Var y l)
		| Iff(x,y) -> (search_Var x l)@(search_Var y l)
	in

	let rec replace_Var : formula -> formula -> formula -> formula
	= fun before after s->
		match s with
		| Var(x) -> if (compare_Var (Var(x)) before) then after else Var(x)
		| True -> True
		| False -> False
		| Neg(x) -> Neg(replace_Var before after x)
		| And(x,y) -> And((replace_Var before after x), (replace_Var before after y))
		| Or(x,y) -> Or((replace_Var before after x), (replace_Var before after y))
		| Imply(x,y) -> Imply((replace_Var before after x), (replace_Var before after y))
		| Iff(x,y) -> Iff((replace_Var before after x), (replace_Var before after y))
	in

	let rec bool_calculation : formula -> bool
	= fun f ->
		match f with
		| True -> true
		| False -> false
		| Neg(x) -> not(bool_calculation x)
		| And(x,y) -> (bool_calculation x)&&(bool_calculation y)
		| Or(x,y) -> (bool_calculation x)||(bool_calculation y)
		| Imply(x,y) -> (not(bool_calculation x))||(bool_calculation y)
		| Iff(x,y) -> (bool_calculation x)==(bool_calculation y)
		| x -> true
	in
	
	let rec sat_calculation : formula -> formula list -> bool
	= fun s l ->
		match l with
		| [] -> (bool_calculation s)
		| hd::tl -> (sat_calculation (replace_Var hd True s) tl)||(sat_calculation (replace_Var hd False s) tl)
	in
sat_calculation f (search_Var f [])
	
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> 

	let rec diff_calculation : aexp * string -> aexp
	= fun (e,x) ->
		match e with
		| Const a ->Const 0
		| Sum(l) -> 
			begin
				match l with
				| [] -> Sum[]
				| [a] -> diff_calculation (a,x)
				| hd::tl -> 
					begin
						match hd with
						| Const _-> (diff_calculation(Sum(tl),x))
						| _ -> Sum(List.append [(diff_calculation(hd,x))] [(diff_calculation(Sum(tl),x))])
					end
			end
		| Power(a,b) ->
			if (compare x a)==0 then 
				begin
					if(b==1) then Const 1
					else Times[Const b;Power(a,b-1)]
				end
			else Const 0
		| Times l ->
			begin
				match l with
				| [] -> Const 0
				| [a] -> diff_calculation(a,x)
				| hd::tl -> 
					begin
						match hd with
						| Const 0 -> Const 0
						| _ -> Sum[Times[hd;diff_calculation(Times(tl),x)];Times[diff_calculation(hd,x);Times(tl)]]
					end
			end
		| Var a -> if(compare x a)==0 then Const 1 else Const 0
	in

	let rec beautiful_diff : aexp -> aexp
	= fun e->
		match e with
		| Const(a) -> Const(a)
		| Var(a) -> Var(a)
		| Power(a,b) -> Power(a,b)
		| Times(l)->
			begin
				match l with
				| [] -> Const 0
				| [a] -> beautiful_diff a
				| hd::tl -> 
					begin
						match hd with
						| Times([]) -> Times([beautiful_diff (Times(tl))])
						| Sum([]) -> Times([beautiful_diff (Times(tl))])
						| Const 1 -> Times([beautiful_diff (Times(tl))])
						| Const 0 -> Const 0
						| _ -> Times(List.append [hd] [(beautiful_diff (Times(tl)))])
					end
			end
		| Sum(l) ->
			begin
				match l with
				| [] -> Const 0
				| [a] -> beautiful_diff a
				| hd::tl ->
					begin
						match hd with
						| Times([]) -> Sum([beautiful_diff (Times(tl))])
						| Sum([]) -> Sum([beautiful_diff (Times(tl))])
						| Const 0 -> Sum([beautiful_diff (Times(tl))])
						| _ -> Sum(List.append [hd] [(beautiful_diff (Times(tl)))])
					end
			end
	in

	beautiful_diff(diff_calculation(e,x))


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> 
	match e with
	| ADD(a,b) -> calculator(a) + calculator(b)
	| SUB(a,b) -> calculator(a) - calculator(b)
	| MUL(a,b) -> calculator(a) * calculator(b)
	| DIV(a,b) -> calculator(a) / calculator(b)
	| SIGMA(a,b,s) ->
		let rec substitute : exp -> int -> exp
		= fun e n ->
			match e with
			| X -> INT(n)
			| ADD(a,b) -> ADD(substitute a n,substitute b n)
			| SUB(a,b) -> SUB(substitute a n,substitute b n)
			| MUL(a,b) -> MUL(substitute a n,substitute b n)
			| DIV(a,b) -> DIV(substitute a n,substitute b n)
			| SIGMA(a,b,c) -> INT(calculator(SIGMA(a,b,c)))
			| INT(a) -> INT(a)
		in
		if (calculator(a)==calculator(b)) then calculator(substitute s (calculator(a)))
		else calculator(SIGMA(INT(calculator(ADD(a,INT(1)))), b, s)) + calculator(substitute s (calculator(a)))
	| X -> 0
	| INT(a) -> a

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec balanced : mobile -> bool
= fun m -> 

	let rec weight : branch -> int
	= fun b ->
		match b with
		| CompoundBranch(k,l) -> 
			begin
				match l with
				| x,y -> (weight x) + (weight y)
			end
		| SimpleBranch(k,l) -> l
	in

	let rec torque : branch -> int
	= fun b ->
		match b with
		| CompoundBranch(a,b) ->
			begin
				match b with
				| x,y -> a * ((weight x) + (weight y))
			end
		| SimpleBranch(a,b) -> a*b
	in
		
	match m with
	| a,b -> 
		let left =
			begin
				match a with
				| CompoundBranch(k,l) -> (balanced l)
				| SimpleBranch(a,b) -> true
			end
		in
		let right =
			begin
				match b with
				| CompoundBranch(k,l) -> (balanced l)
				| SimpleBranch(a,b) -> true
			end
		in
		((torque a)==(torque b)) && left && right

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let del_tail l = 
		match l with
		| [] -> []
		| _ ->List.rev (List.tl (List.rev l))
	in

	let rec compareVs v1 v2 = 
		match v1, v2 with
		| [], []       -> true
		| [], _
		| _, []        -> false
		| x::xs, y::ys -> x = y && compareVs xs ys
	in

	let rec badd : bin -> bin -> bin -> bin
	= fun b1 b2 x->
		if ((compareVs b1 [ZERO])||(compareVs b1 []))  then
			begin
				if (compareVs x [ZERO]) then b2 else badd b2 x [ZERO]
			end
		else if ((compareVs b2 [ZERO])||(compareVs b2 [])) then
			begin
				if (compareVs x [ZERO]) then b1 else badd b1 x [ZERO]
			end
		else
			let b1_tail = (List.hd (List.rev b1)) in
			let b2_tail = (List.hd (List.rev b2)) in
			let rec counting_one l n
				= match l with
				| [] -> n
				| [ZERO] -> n
				| [ONE] -> n+1
				| hd::tl -> if hd==ONE then counting_one tl n+1 else counting_one tl n
			in
			let cnt = (counting_one ([b1_tail;b2_tail]@x) 0) in
			if cnt==0 then (badd (del_tail b1) (del_tail b2) [ZERO])@[ZERO]
			else if cnt==1 then (badd (del_tail b1) (del_tail b2) [ZERO])@[ONE]
			else if cnt==2 then (badd (del_tail b1) (del_tail b2) [ONE])@[ZERO]
			else (badd (del_tail b1) (del_tail b2) [ONE])@[ONE]
	in

match (List.rev b2) with
| [ONE] -> b1
| [ZERO] -> [ZERO]
| [] -> [ZERO]
| hd::tl ->
	begin
		match hd with
		| ZERO -> (bmul b1 tl)@[ZERO]
		| ONE -> badd ((bmul b1 tl)@[ZERO]) b1 [ZERO]
	end