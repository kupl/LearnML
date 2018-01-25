type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (aexp,x) -> match aexp with 
	| Sum list1 -> (match list1 with | hd::tl -> hd) (*No idea why it is not oworking*) 
	| Const x -> Const x
	| Var y -> Var y
	| Power (z1,z2) -> if z2=2 then (Times [Const z2; Var z1]) else (Times [Const (z2); Power(z1, z2-1)]);
	| Times list2 -> Const 2

