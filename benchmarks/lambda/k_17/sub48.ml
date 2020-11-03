(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type env = var list


let rec check_2 : env -> lambda -> bool
= fun env lamb ->
begin
	match lamb with
	|V v -> 
	begin
		match env with
		|[] -> false
		|hd::tl -> if v = hd then true else check_2 tl lamb
	end
	|P (v, lamb') -> check_2 ([v]@env) lamb'
	|C (lamb_1, lamb_2) -> (check_2 env lamb_1) && (check_2 env lamb_2)
end

let rec check : lambda -> bool
= fun lam -> check_2 [] lam



