(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> (* TODO *)
	let rec chk lam scp =
		match lam with
		| V x ->
		begin
			try
				(fun _ -> true) @@ List.find (fun y -> if x=y then true else false) scp
			with
				_ -> false	
		end
		| P (x,l) -> chk l (x::scp)
		| C (l1,l2) ->
			chk l1 scp && chk l2 scp
	in
	chk lam []
