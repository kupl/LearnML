(*problem 2*)
type lambda = 
	|V of var
	|P of var * lambda
	|C of lambda * lambda

let rec classify lam li =
match lam with
|P (v,l) -> classify l (li @ [v])
|C(l1,l2) -> classify l1 li && classify l2 li
|V a -> 
(match li with
|[]->false
|hd::tl -> if hd = a then true else classify lam tl
)

let rec check : lambda -> bool
= fun lam ->  
	classify lam [] 