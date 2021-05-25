exception Error of string

(* EX8 : check *)
type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
			and var = string
(* definition of lambda *)

let rec check a =
	(* function check : check the lambda is well formed using list that has vars of areas as an element *)
	let rec check2 mt lst =
		match mt with
			V var -> List.mem var lst
			(* check the var of the station is a member of list *)
			| P ( nm, mt ) -> check2 mt ( nm :: lst )
			(* add var into the list and check lambda recursively *)
			| C ( m1, m2 ) -> ( check2 m1 lst ) && ( check2 m2 lst ) in
			(* check all of two lambda are well formed *)
	check2 a []
