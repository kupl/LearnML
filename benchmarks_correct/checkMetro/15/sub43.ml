type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

(*
let rec arealist lambdal =
	match lambdal with
	| V i -> []
	| P (i1, i2) -> (if List.mem i1 lambdal then arealist i2 
				else i1 :: arealist i2)
	| C (i1, i2) -> arealist i1 :: arealist i2
*)

(*
let changeMetro list1 =
	let _ list2 : (list1 * list)
*)
(*
let rec makearea : lambda -> 'a list = function list1 ->
	match list1 with
	| P (i1, i2) -> i1 :: makearea i2
	| V i -> []
	| C (i1, i2) -> makearea i1 @ makearea i2
*)

let rec checkf (lambdal, list1)  =
	match lambdal with
	| V i -> (if List.mem i list1 then true else false)
	| C (i1, i2) -> checkf (i1,list1) && checkf (i2,list1)
	| P (i3, i4) -> checkf (i4, i3::list1)

let check lambdal = checkf (lambdal, [])
