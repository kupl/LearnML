(*********************)
(*     Problem 1     *)
(*********************)

let rec fold f l a =
match l with
|[] -> a
|hd::tl -> f hd (fold f tl a)

let rec max : int list -> int 
= fun l ->
if l==[] then raise (Failure "List is empty")
else  fold (fun a b -> if a>b then a else b) l min_int

let rec min : int list -> int
= fun l -> 
if l==[] then raise (Failure " List is empty")
else fold (fun a b -> if a<b then a else b) l max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter p l =
match l with
|[] -> []
|hd::tl -> if p hd ==true then hd::filter p tl
           else filter p tl
(*********************)
(*     Problem 3     *)
(*********************)
let double f a = 
f ( f a )

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree
let rec mem n t =
match t with
|Empty -> false
|Node (k,t1,t2) -> (k==n)||(mem n t1)||(mem n t2)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd x y =
match y with
|ZERO -> x
|SUCC (y1) -> SUCC (natadd x y1)

let rec natmul x y =
match y with
|ZERO -> ZERO
|SUCC (y1) -> natadd x (natmul x y1)

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec equal_help exp=
match exp with
|Num n -> n
|Plus (exp1,exp2) -> (equal_help exp1) + (equal_help exp2)
|Minus (exp1,exp2) -> (equal_help exp1) - (equal_help exp2)

let rec eval f =
match f with
|True -> true
|False -> false
|Not f1 -> if eval f1 == true then false
           else true
|AndAlso (f1, f2) -> if (eval f1)&&(eval f2)== true then true
                     else false
|OrElse (f1,f2) -> if (eval f1)||(eval f2) == true then true
                   else false
|Imply (f1,f2) -> if ((eval f1)==true)&&(eval f2==false) then false
                  else true
|Equal (exp1,exp2)-> if (equal_help exp1)==(equal_help exp2) then true
                     else false
