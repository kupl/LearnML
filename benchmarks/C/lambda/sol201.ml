exception TODO (*done done*)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec listSearch ((l: string list), (key: string)): bool= 
	match l with
	| [] -> false
	| hd::tl -> if hd = key then true else listSearch (tl, key)

let rec checkNameStation ((n:string list), (m:lambda)): bool= 
	match m with
	| V svar -> listSearch (n, svar)
	| P (svar, subm) -> checkNameStation(svar::n, subm)
	| C (subm1, subm2) -> checkNameStation (n, subm1) && checkNameStation(n, subm2)

let rec check (m: lambda): bool = 
	match m with
	| V n -> false
	| P (n, V svar) -> if n = svar then true else false
	| P (n, subm) -> checkNameStation ([n], subm)
	| C (subm1, subm2) -> check (subm1) && check (subm2)
