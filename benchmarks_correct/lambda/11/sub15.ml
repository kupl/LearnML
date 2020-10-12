(* 2008-11874 Lee, Sujee *)
(* EXERCISE 7 *)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check lambda = (* check : lambda -> bool = <fun> *)
	let rec takeArea(lambda,idlist) = (* lambda : lambda / idlist : string list*)
		match (lambda,idlist) with
			| (V id, idlist) -> List.mem id idlist
			| (P(id,met),idlist) -> takeArea(met,(List.append [id] idlist))
			| (C(met1,met2),idlist) -> (takeArea(met1,idlist)) && (takeArea(met2,idlist))
		in
	match lambda with
		| V id -> false (* if lambda contains only V, then false. *)
		| P(id,met) -> takeArea(met,[id])
		| C(met1,met2) -> (check(met1)) && (check(met2))

