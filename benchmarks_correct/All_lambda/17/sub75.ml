type lambda = V of var
		|P of var * lambda
		|C of lambda * lambda
and var = string

let rec checkHelper ((m : lambda), (l : string list)) : bool = 
    match m with 
    | V(s0) -> List.mem s0 l
    | P(s0, m0) -> checkHelper(m0, (s0::l))
    | C(m1, m2) -> checkHelper(m1, l) && checkHelper(m2, l)

let check (m : lambda) : bool =
	checkHelper(m, [])
