(* Helper functions *)
let rec map f l =
    match l with
    | [] -> []
    | head::tail -> (f head)::(map f tail);;

let rec fold f l acc =
    match l with
    | [] -> acc
    | head::tail -> f head (fold f tail acc);;

let (--) start halt = 
    let rec aux n acc =
        if n < start then acc else aux (n - 1) (n::acc)
    in aux halt [];;

let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
    fold (+) (map f (a--b)) 0;;

(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst
= match lst with
    | [] -> []
    | head::tail -> if pred head
        then head::(filter pred tail)
        else filter pred tail;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (list1,list2) -> 
match list1 with
    | [] -> list2
    | head1::tail1 -> (match list2 with
        | [] -> list1
        | head2::tail2 -> head1::head2::(zipper (tail1,tail2)));;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
match n with
    | 0 -> (function x -> x)
    | count -> (function x -> f (iter(count-1,f) x));;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
=fun (aexp,x) ->
    match aexp with
    | Const _ -> Const 0
    | Var variable when variable = x -> (Const 1)
    | Var variable -> (Const 0)
    | Power (variable,0) -> (Const 0)
    | Power (variable,power) when x = variable -> Times [Const power; Power (variable,power-1)]
    | Power (variable,power) -> (Const 0)
    | Sum [] -> Const 0
    | Sum [e] -> diff(e,x)
    | Sum (head::tail) -> (Sum [diff(head,x); diff((Sum tail),x)])
    | Times [] -> Const 0
    | Times [e] -> diff(e,x)
    | Times (head::tail) -> Sum [Times (diff(head,x)::tail)
                                ;Times [head; diff((Times tail),x)]];;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec fcal : exp -> (int -> int)
=fun e -> match e with
    | X -> (function x -> x)
    | INT n -> (function _ -> n)
    | ADD (e1,e2) -> (function x -> ((fcal e1) x) + ((fcal e2) x))
    | SUB (e1,e2) -> (function x -> ((fcal e1) x) - ((fcal e2) x))
    | MUL (e1,e2) -> (function x -> ((fcal e1) x) * ((fcal e2) x))
    | DIV (e1,e2) -> (function x -> ((fcal e1) x) / ((fcal e2) x))
    | SIGMA (e1,e2,e3) -> (function x -> sigma (fcal e3) ((fcal e1) 0) ((fcal e2) 0));;

let calculator : exp -> int
=fun e ->
    if ((fcal e) 0) = ((fcal e) 1) then ((fcal e) 0) else raise (Failure "Expression has no definite value.");;

(* Tests *)
(*
open Printf
*)

(* Test 1 *)
(*
let test1 = filter (fun x -> x mod 2 = 0) [1;2;3;4;5];;
List.iter (printf "%d ") test1;;
printf "\n";;
let test2 = filter (fun x -> x > 0) [5;-1;0;4;-9;3;2;1];;
List.iter (printf "%d ") test2;;
printf "\n";;
*)

(* Test 2 *)
(*
let test1 = zipper ([1;3;5],[2;4;6]);;
List.iter (printf "%d ") test1;;
printf "\n";;
let test2 = zipper ([1;3],[2;4;5;6]);;
List.iter (printf "%d ") test2;;
printf "\n";;
let test3 = zipper ([1;3;5;6],[2;4]);;
List.iter (printf "%d ") test3;;
printf "\n";;
*)

(* Test 3 *)
(*
if iter(7, fun x -> 2+x) 0 != 14 then print_string("fail");;
*)

(* Test 4 cases *)
(*
let test0 = Const 1;;
let test1 = Sum [Const 2; Var "x"];;
let test2 = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
let test3 = Sum [Power ("x", 0); Times [Const 0; Var "x"]; Const 0];;
*)

(* Test 5 *)
(*
let test0 = fcal (INT 1);;
printf "%d " (test0 0);;
printf "\n";;
let test1 = fcal (MUL(INT 2, INT 1));;
printf "%d " (test1 0);;
printf "\n";;
let test2 = fcal (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
printf "%d " (test2 0);;
printf "\n";;
*)

(*
let test0 = calculator (INT 1);;
printf "%d " (test0);;
printf "\n";;
let test1 = calculator (MUL(INT 2, INT 1));;
printf "%d " (test1);;
printf "\n";;
let test2 = calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;
printf "%d " (test2);;
printf "\n";;
*)
