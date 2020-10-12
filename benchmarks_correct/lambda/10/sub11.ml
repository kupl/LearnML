type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
	and var = string

let rec check met =
	check2 met []
	and check2 met strs = 
	match met with
	V n -> if List.mem n strs	then true else false
	|P (n,m) -> check2 m (n::strs)
	|C (m1, m2) -> (check2 m1 strs) && (check2 m2 strs)
	
