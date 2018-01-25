(*********************) (* Problem 1: filter *) (*********************) let rec filter pred lst = 
match lst with
|[] -> []
|hd::tl -> if pred hd then hd::filter pred tl
else filter pred tl;;
(*********************) (* Problem 2: zipper *) (*********************) let rec zipper : int list * int list -> int list =fun (a,b) ->
match a with 
|[]->b
|hd::tl -> addlist hd (zipper (tl,b));

let rec addlist x l =
match l with 
|[]->[x]
|hd::tl -> if x>hd then hd::addlist x tl
else x::hd::tl;;

(*

(*******************) (* Problem 3: iter *) (*******************) let rec iter : int * (int -> int) -> (int -> int) =fun (n,f) ->
 


(*********************) (* Problem 4: Diff *) (*********************) type aexp = | Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list let rec diff : aexp * string -> aexp =fun (aexp,x) -> aexp (* TODO *) 

(*************************)(* Problem 5: Calculator *) (*************************) type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp let calculator : exp -> int =fun e -> 0 (* TODO *) 

*)