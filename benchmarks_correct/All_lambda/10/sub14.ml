exception Error of string
type lambda = V of var | P of var * lambda | C of lambda * lambda
	and var = string

let rec env_checker : lambda -> string list -> bool =
= fun mymet myls ->
			(match mymet with
				V sta -> if (List.mem sta myls) then true else false
				| P (thevar, themet) -> if (List.mem thevar myls) then (env_checker themet myls) 
								else (env_checker themet (thevar :: myls))
				| C (bm, cm) -> ((env_checker bm myls) && (env_checker cm myls)))

let check alp =
		(env_checker alp [])