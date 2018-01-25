(* Problem 1 *)
let rec fib n =
    match n with
    0 -> 0
    |1 -> 1
    |_ -> fib (n-1) + fib(n-2) ;;

 (* TODO *)

(* Problem 2 *)
let rec factorial a =
    match a with
    1 -> 1
    | 0 -> 1
    |_ -> a* factorial (a-1) ;;

let permutation x y = (factorial x) / (factorial y) ;;

let pascal (x,y) = (permutation x y) / (factorial y ) ;;

 (* TODO *)

pascal (0,0) ;;
pascal (1,0) ;;
pascal (1,1) ;;
pascal (2,1) ;;
pascal (4,2) ;;

(* Problem 3 *)

let rec mark x y =
    if x mod y = 0 then 0
    else 1 ;;

let rec sigma mark a b =
    if b = 1 then 0
   else mark a b + (sigma mark a (b-1)) ;;

let prime x =
    if x = 1 then false
    else if sigma mark x x = (x-2) then true else false ;;

 (* TODO *)

prime 3 ;;
prime 4 ;;
prime 2 ;;
prime 1 ;;
prime 11 ;;
prime 44 ;;
prime 173 ;;
prime 10 ;;

(* Problem 4 *)
let rec sigma f a b =
    if b = a then f b
    else f b + (sigma f a (b-1)) ;;

 (* TODO *)

sigma (fun x -> x) 1 10 ;;
sigma (fun x -> x*x) 1 7 ;;
sigma (fun x -> x*x*x) 2 3 ;;
