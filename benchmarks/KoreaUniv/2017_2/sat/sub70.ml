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