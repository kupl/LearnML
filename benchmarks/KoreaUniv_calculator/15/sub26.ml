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