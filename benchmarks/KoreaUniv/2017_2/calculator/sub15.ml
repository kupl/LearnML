(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec helpsigma
= fun h -> match h with
	| X -> (fun x -> x)
	| INT n -> (fun x -> n)
	| ADD (a,b) -> (fun x -> ((helpsigma a) x + (helpsigma b) x))
	| SUB (a,b) -> (fun x -> ((helpsigma a) x - (helpsigma b) x))
	| MUL (a,b) -> (fun x -> ((helpsigma a) x * (helpsigma b) x))
	| DIV (a,b) -> (fun x -> ((helpsigma a) x / (helpsigma b) x))
	| SIGMA (a,b,c) -> raise (Failure "error")

let rec operation
= fun opr -> match opr with
	| X -> raise (Failure "error")
	| INT n -> n
	| ADD (n1,n2) -> (operation n1) + (operation n2) 
	| SUB (n1,n2) -> (operation n1) - (operation n2) 
	| MUL (n1,n2) -> (operation n1) * (operation n2)
	| DIV (n1,n2) -> (operation n1) / (operation n2)
	| SIGMA (n1,n2,n3) -> let rec sigma
				= fun k1 k2 -> if k1>k2 then 0
						else if k1=k2 then (helpsigma n3) k1
						else (helpsigma n3) k1 + sigma (k1+1) k2
				in sigma (operation n1) (operation n2)