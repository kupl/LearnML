type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string
let check met = 
	let rec cm nowl mect =
		match mect with
		|V a -> (List.mem a nowl)
		|P(n,m) -> (cm (n::nowl) m)
		|C(a,b)-> (cm nowl a) && (cm nowl b)
	in
	cm [] met


