(*********************) (* Problem 1: filter *) (*********************) 
let rec filter pred lst = match lst with
|[]->[]
|hd::tl -> if (pred hd) then [hd]@(filter pred tl) else filter pred tl;;
(*********************) 
(* Problem 2: zipper *) (*********************) 
let rec zipper : int list * int list -> int list =fun (a,b) -> 
match a,b with
|[],_ -> b
|_,[] -> a
|hda::tla, hdb:: tlb -> if hda<hdb then hda :: zipper(tla,b) else hdb::zipper(a,tlb);; 
(*******************)
 (* Problem 3: iter *) (*******************) 
let rec iter : int * (int -> int) -> (int -> int) =fun (n,f) x->
if n = 0 then x else iter(n-1,f) (f x);;
(*********************) 
(* Problem 4: Diff *) (*********************) 
type aexp = 
| Const of int 
| Var of string 
| Power of string * int 
| Times of aexp list 
| Sum of aexp list 
let rec diff : aexp * string -> aexp =fun (aexp,x) -> match aexp with
|Const y -> Const 0
|Var t -> if t = x then Const 1 else Const 0
|Power (s,i) -> Times[Const i;Power(s,i-1)]
|Times(k) -> (match k with 
[] -> Const 0
|hd::tl->Sum([Times([diff(hd,x)]@tl)]@[Times([hd]@[diff(Times(tl),x)])]))
|Sum(k) -> (match k with
[]->Const 0
|h::t -> Sum([diff(h,x)]@[Sum([diff(Sum(t),x)])]));;
 (************ *************) 
(* Problem 5: Calculator *) (*************************) 
type exp =
 X 
| INT of int 
| ADD of exp * exp 
| SUB of exp * exp 
| MUL of exp * exp 
| DIV of exp * exp 
| SIGMA of exp * exp * exp 
let calculator : exp -> int = fun exp -> 
	let rec cal exp num fl result = (
match exp with
X-> if fl = 0 then raise(Failure "match failure exception") else num
|INT(k) -> k
|ADD(x,y)-> (cal x num fl result) + (cal y num fl result)
|SUB(x,y)-> (cal x num fl result) - (cal y num fl result)
|MUL(x,y)-> (cal x num fl result)*(cal y num fl result)
|DIV(x,y)-> if (cal y num fl result) = 0 then raise(Failure "divide by zero") else (cal x num fl result)/(cal y num fl result)
|SIGMA(x,y,z)-> 
		let num = cal x num fl result in
			let fl = 1 in
				(if (cal x num fl result)>(cal y num fl result) then result
					else let result = result + (cal z num fl result) in
						cal (SIGMA(INT ((cal x num fl result)+1),INT (cal y num fl result), z)) num fl result)
) in cal exp 0 0 0;;
