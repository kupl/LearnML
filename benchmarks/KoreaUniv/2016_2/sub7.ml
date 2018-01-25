(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
      match l with
        | [] -> a
          |hd::tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> fold (fun a b ->  if a > b then a else b) lst min_int;;

let rec min : int list -> int
= fun lst -> fold (fun a b -> if a > b then b else a) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
    match lst with
    | [] -> []
    | h::t -> if pred h then h::filter pred t
     else filter pred t;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

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
          | Node (num, sub_left, sub_right) -> if n = num then true else (mem n sub_left) || (mem n sub_right);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
        | ZERO
            | SUCC of nat

let rec natadd : nat -> nat -> nat
    = fun n1 n2 ->
                    match n2 with
                                | ZERO -> n1
                                            | SUCC n -> SUCC(natadd n1 n);;


let rec natmul : nat -> nat -> nat
    =fun n1 n2 ->
            match n2 with
                | ZERO -> ZERO
                    | SUCC n -> natadd n1 (natmul n1 n);;



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

let rec eval : formula -> bool
= fun f ->
      match f with
          | True -> true
              | False -> false
                  | Not x -> not (eval x)
                      | AndAlso (x, y) -> (eval x) && (eval y)
                          | OrElse (x, y) -> (eval x) || (eval y)
                              | Imply(x, y) -> (not (eval x)) || (eval y)
                                  | Equal(x, y) ->
                                                let rec exp_to_int = fun n1->
                                                            match n1 with
                                                                      | Num x -> x
                                                                                | Plus (x, y) -> (exp_to_int x) + (exp_to_int y)
                                                                                          | Minus (x, y) -> (exp_to_int x) - (exp_to_int y)
                                                                                                      in (exp_to_int x) = (exp_to_int y);;
