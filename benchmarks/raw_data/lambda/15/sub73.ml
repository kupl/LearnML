type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string


let rec checkarea (a: string list) (s: string): bool = 
	match a with
	| [] -> false
	| h::t -> if (s = h) then true else (checkarea t s)

let rec areacount (m: lambda) (a: string list): bool =
	match m with
	| V s -> (checkarea a s)
	| P (s, m) -> areacount m (s::a)
	| C (m1, m2) -> (areacount m1 a) && (areacount m2 a)

	
let rec check (m: lambda): bool = 
	areacount m []
