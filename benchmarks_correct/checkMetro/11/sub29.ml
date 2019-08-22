type lambda = V of var
	|P of var * lambda
	|C of lambda * lambda
and var = string
exception NotArea

let rec check x =

	let rec deleteElem a lst =		(* delete element in the list *)
		match lst with
		|[] -> []
		|[hd] -> if a=hd then [] else [hd]
		|hd::tl -> if a=hd then (deleteElem a tl)
			else hd::(deleteElem a tl)
	in

	let rec mkstrlst t =			(* make station list and delete area vars *)
		match t with
		V a -> [a]
		|P (a, b) -> (deleteElem a (mkstrlst b))
		|C (a, b) -> (mkstrlst a)@(mkstrlst b)
	in

	if (mkstrlst x)=[] then true else false
