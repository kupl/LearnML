type lambda = V of var
	|P of var * lambda
	|C of lambda * lambda
  and var = string

let rec check lst var lambda =
	
	let mklst lst x = x::lst
	in
	match lambda with	
	|V n -> if (List.mem n lst) then true else false
	|P(n,m) -> (check (mklst lst n) var m) || (check (mklst lst n) n m)
	|C(m1,m2) -> (check lst var m1) && (check lst var m2)

let check lambda = match lambda with
	|V n -> false
	|P(n,m) -> (check [n] n m)
	|C(m1,m2) -> false
