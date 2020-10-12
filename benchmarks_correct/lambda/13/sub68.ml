type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string
let rec com(l,n) =
	match l with
	| []->false
	| head::tail -> ((head = n) || com(tail,n))
let rec cMetro(l, m) = 
	match m with
	| V n -> com(l,n)
	| P(n,mm) -> cMetro(n::l,mm)
	| C(m1,m2) -> (cMetro(l,m1)) && (cMetro(l,m2))
let rec check(m)=
	cMetro([],m)
