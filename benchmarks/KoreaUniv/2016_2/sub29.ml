(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
    match l with
    | [] -> a
    | hd::tl -> f hd (fold f tl a);;

(* ASSUMPTION: LIST IS NON-EMPTY *)
let rec max : int list -> int
= fun lst -> fold (fun x y -> if (x > y) then x else y) lst 0;;

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x < y then x else y) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
    match lst with
    | [] -> []
    | hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl);;

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
= fun n tree -> match tree with 
    | Empty -> false
    | Node (value, left, right) -> if value = n then true else ((mem n left) || (mem n right));; 

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
    | ZERO
    | SUCC of nat

let rec natval n = 
    match n with
    | ZERO -> 0
    | SUCC s -> 1 + natval s;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1, n2 with
    | ZERO, _ -> n2
    | _, ZERO -> n1
    | SUCC v1, SUCC v2 -> natadd v1 (SUCC n2);;

let rec natmul_helper n1 n2 result =
    match n1, n2 with 
        | _, ZERO -> result
        | ZERO, _ -> result
        | SUCC v1, _ -> natmul_helper v1 n2 (natadd result n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> natmul_helper n1 n2 ZERO;;

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

let rec eval_exp e =
    match e with
        | Num i -> i
        | Plus (e1, e2) -> eval_exp e1 + eval_exp e2 
        | Minus (e1, e2) -> eval_exp e1 - eval_exp e2;; 

let rec eval : formula -> bool
= fun f -> match f with 
    | True -> true
    | False -> false
    | Not f1 -> 
        let v1 = eval f1 in 
        begin
            match v1 with 
                | true -> false
                | false -> true
        end
    | AndAlso (f1, f2) -> 
        let v1 = eval f1 in
        let v2 = eval f2 in 
        begin
            match v1, v2 with
                | true, true -> true
                | _, _ -> false
        end
    | OrElse (f1, f2) -> 
        let v1 = eval f1 in
        begin
            match v1 with 
                | true -> true
                | _ -> let v2 = eval f2 in 
                    begin
                        match v2 with 
                            | true -> true
                            | _ -> false
                    end
        end
    | Imply (f1, f2) -> 
        let v1 = eval f1 in
        let v2 = eval f2 in 
        begin
            match v1, v2 with 
                | true, _ -> v2
                | _, _ -> true
        end
    | Equal (e1, e2) -> eval_exp e1 = eval_exp e2;;