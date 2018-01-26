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

type new_bool =
  | T
  | F
  | Undef of string
  | NegUndef of string

let rec sat_rec : formula -> new_bool
= fun f ->
	match f with
	| True -> T
	| False -> F
	| Var v -> Undef v
	| Neg f -> (
		let nb = sat_rec f in (* nb: new bool *)
		match nb with
		| T -> F
		| F -> T
		| Undef v -> NegUndef v
		| NegUndef v -> Undef v
	)
	| And (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		if nb1=F || nb2=F then F
		else if nb1=T && nb2=T then T
		else if nb1=T then nb2
		else if nb2=T then nb1
		else ( (* nb1 and nb2 are both not one of T or F*)
			match nb1 with
			| Undef v1 -> (
				match nb2 with
				| Undef v2 -> T
				| NegUndef v2 -> if v1=v2 then F else T
				| _ -> T
			)
			| NegUndef v1 -> (
				match nb2 with
				| Undef v2 -> if v1=v2 then F else T
				| NegUndef v2 -> T
				| _ -> T
			)
			| _ -> T
		) 
	)
	| Or (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		if nb1=F && nb2=F then F else T
	)
	| Imply (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		match nb1 with
		| T -> nb2
		| F -> T
		| Undef v1 -> (
			match nb2 with
			| NegUndef v2 -> if v1=v2 then Undef v1 else T
			| _ -> T
		)
		| NegUndef v1 -> (
			match nb2 with
			| Undef v2 -> if v1=v2 then NegUndef v1 else T
			| _ -> T
		)
	)
	| Iff (f1, f2) -> (
		let (nb1, nb2) = ((sat_rec f1), (sat_rec f2)) in
		match nb1 with
		| T -> nb2
		| F -> (
			match nb2 with
			| T -> F
			| F -> T
			| Undef v2 -> NegUndef v2
			| NegUndef v2 -> Undef v2
		)
		| Undef v1 -> (
			match nb2 with
			| T -> nb1
			| F -> NegUndef v1
			| Undef v2 -> T
			| NegUndef v2 -> if v1=v2 then F else T
		)
		| NegUndef v1 -> (
			match nb2 with
			| T -> Undef v1
			| F -> nb1
			| Undef v2 -> if v1=v2 then F else T
			| NegUndef v2 -> T
		)
	)
;;

let sat : formula -> bool
= fun f -> 
	let sat_result = sat_rec f in
	if sat_result=F then false else true
;;
