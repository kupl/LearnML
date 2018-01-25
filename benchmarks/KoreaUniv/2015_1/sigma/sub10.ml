exception WrongInput

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a>b then raise WrongInput else match b-a with
0 -> f a
|_ -> sigma f (a+1) b + f a;;
