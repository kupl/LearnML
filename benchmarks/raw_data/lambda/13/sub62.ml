type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string


let rec asdf met l1 = 
	match met with
	| V a -> if (List.mem a l1)=true then true else false
	| C (m1, m2) -> (asdf m1 l1)&&(asdf m2 l1)
	| P (ne, mt) -> (asdf mt (ne::l1))

let rec check m =
        match m with
        | V a -> false
        | P (var, lambda) -> asdf m []
        | C (m1, m2) -> (asdf m1 [])&&(asdf m2 [])


