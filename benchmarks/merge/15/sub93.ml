(* Exercise 1 *)
let rec merge a b =
	match a with
	| [] -> b
	| ahead::atail -> 
	(
		match b with
		| [] -> a
		| bhead::btail -> if ahead>bhead then ahead::(merge atail b) else bhead::(merge a btail)
	)


(* Exercise 2 *)
let sigma a b f= 
	let rec range a b = if a>b then [] else a::range (a+1) b in
	List.fold_left (+) 0 ( List.map f ( range a b ))



(* Exercise 3 *)
let rec iter (n, f)=
	match n with
	| 0 -> fun x -> x 
	| n -> fun x -> f (iter (n-1, f) x) 


(* Exercise 4 *)
type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr*expr
	| MINUS of expr*expr

let rec calc e =
	match e with
	| NUM n -> n
	| PLUS (e1, e2) -> (calc e1) + (calc e2)
	| MINUS(e1, e2) -> (calc e1) - (calc e2)

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> if eval f = true then eval FALSE else eval TRUE
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> if eval f1 = true then eval f2 else eval TRUE
	| LESS (e1, e2) -> if calc ( MINUS (e1,e2)) < 0 then eval TRUE else eval FALSE

(* Exercise 5 *)
type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n1 with
	| ZERO -> n2
	| SUCC n1_pred -> natadd (n1_pred, SUCC n2)

let rec natmul (n1, n2) =
	match n1 with
	| ZERO -> ZERO
	| SUCC n1_pred -> natadd (n2, natmul (n1_pred, n2))

