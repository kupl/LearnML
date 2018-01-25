(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
      match lst with
        | [] -> 0
        | hd::tl -> let getmax x y = if x > y then x else y in
          getmax hd (max tl);;


let rec min : int list -> int
= fun lst -> 
    match lst with
      | [] -> max_int
      | hd::tl -> let getmin x y = if x < y then x else y in
        getmin hd (min tl);;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
  match lst with
  | [] -> []
  | hd::tl -> if (pred hd) then [hd] @ (filter pred tl) else (filter pred tl);;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f ( f a );;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
        match tree with
          | Empty -> false
          | Node (num, left, right) ->
            if num = n then true else ((mem n left) || (mem n right)) ;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec n_to_i x =
  match x with
  | ZERO -> 0
  | SUCC y -> 1 + (n_to_i y);;

let rec i_to_n x =
  match x with
  | 0 -> ZERO
  | _ -> SUCC (i_to_n (x-1));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> i_to_n ( (n_to_i n1) + (n_to_i n2) );;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> i_to_n ( (n_to_i n1) * (n_to_i n2) );;


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
  | Minus of exp * exp;; 

let rec evalExp : exp -> int
= fun x -> match x with
| Num a -> a
| Plus (a,b) -> (evalExp a) + (evalExp b)
| Minus (a,b) -> (evalExp a) - (evalExp b);;

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not k -> if (eval k) then false else true
| AndAlso (a, b) -> if ( (eval a) && (eval b) ) then true else false
| OrElse (a, b) -> if ( (eval a) || (eval b) ) then true else false
| Imply (a, b) -> if ( (eval a) && (eval (Not b)) ) then false else true
| Equal (a, b) -> if ( (evalExp a) = (evalExp b) ) then true else false;;
