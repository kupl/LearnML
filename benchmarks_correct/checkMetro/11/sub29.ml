type metro = STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
and name = string
exception NotArea

let rec checkMetro x =

	let rec deleteElem a lst =		(* delete element in the list *)
		match lst with
		|[] -> []
		|[hd] -> if a=hd then [] else [hd]
		|hd::tl -> if a=hd then (deleteElem a tl)
			else hd::(deleteElem a tl)
	in

	let rec mkstrlst t =			(* make station list and delete area names *)
		match t with
		STATION a -> [a]
		|AREA (a, b) -> (deleteElem a (mkstrlst b))
		|CONNECT (a, b) -> (mkstrlst a)@(mkstrlst b)
	in

	if (mkstrlst x)=[] then true else false
