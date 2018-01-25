(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
match lst with
        | [] -> []
        | hd::tl -> if pred hd then hd::filter pred tl else filter pred tl;;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a, b with
        | [],_ -> b
        | _,[] -> a
        | hd1::tl1, hd2::tl2 -> hd1::hd2::zipper (tl1, tl2);;

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n, f) x -> if n == 0 then x 
        else (if x > 0 then iter (n - 1, f) (f x ) 
        else failwith "Error");;

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp, x) -> match aexp with
        | Const a -> Const 0
        | Var "x" -> Const 1
        | Power ("x", a) -> match a with
                | 0 -> Const 0
                | 1 -> Const 1
                | _ -> Times[Const a; Power ("x", a - 1)]
        |Times l -> match l with

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

exception Error

let calculator : exp -> int
=fun e -> let rec calc e env = match (e, env) with
        | (X, []) -> raise Error
        | (X, hd::tl) -> hd
        | (INT i,_) -> i
        | (ADD (a, b), env) -> calc a env + calc b env
        | (SUB (a, b), env) -> calc a env - calc b env
        | (DIV (a, b), env) -> if calc b env == 0 then raise Error else calc a env / calc b env
        | (MUL (a, b), env) -> calc a env * calc b env
        | (SIGMA (a, b, c), env) ->
                if calc a env > calc b env then raise Error
	        else if (calc b env - calc a env) < 1 then (calc c (calc a env::env))
		else (calc c (calc a env::env)) + (calc (SIGMA (INT (calc a env + 1), INT (calc b env), c)) env)
	in calc e [] ;;
