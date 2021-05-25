type lambda = 
	V of var
	|P of var * lambda
	|C of lambda * lambda
and var = string

let rec check mt = ckM mt []
and ckM mt l =
	match mt with
	V(nm)				-> if_cont nm l
	|P(nm, mt_a)			-> ckM mt_a (nm::l)
	|C(mt_a, mt_b)	-> (ckM mt_a l) && (ckM mt_b l)
and if_cont nm l = List.exists (equal nm) l
and equal nm_a nm_b = if nm_a = nm_b then true else false
