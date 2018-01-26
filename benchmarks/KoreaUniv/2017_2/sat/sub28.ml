(*problem3*)
type formula =
	TRUE
	| FALSE
	| Var of string
	| Neg of formula
	| And of formula * formula
	| Or of formula * formula
	| Imply of formula * formula
	| Iff of formula * formula

let trans_tr : string -> bool
= fun str->  
match str with 
|_ ->true


let trans_fl : string -> bool 
= fun str->  
match str with 
|_ -> false


let rec decide : formula -> bool
= fun f ->
match f with
| TRUE -> true
| FALSE -> false
| Var str ->  
		trans_tr str
| Neg f -> not (decide f)
| And (f1,f2) ->(((decide f1)&&(decide f2)) || ((decide (Neg f1))&&(decide f2) ) || ((decide f1)&& (decide (Neg f2))) || ((decide (Neg f1))&&(decide (Neg f2))))
| Or (f1,f2) -> (((decide f1)||(decide f2)) || ((decide (Neg f1))||(decide f2))  || ((decide f1)||(decide (Neg f2))) || ((decide (Neg f1))||(decide (Neg f2))) )
| Imply (f1,f2)-> if (decide f1) = false then true else (decide f2) 
| Iff (f1,f2)-> if ((decide f1) = (decide f2)) then true else false 

let rec sat :formula-> bool
= fun f->
 if (decide f = false) then false
else true
