open Printf
let rec range a b =
        if a > b then []            (* Base case *)
        else a :: range (a+1) b     (* Recursive case *)

(* Problem 1 *)
let rec fib : int -> int
= fun n ->
        match n with
        0 -> 0
        | 1 -> 1
        | _ -> fib (n-1) + fib (n-2);;

let f elem = 
        printf "fib %d: %d\n" elem (fib elem) in
        List.iter f (range 1 10);;

printf "-----------------------------------------\n";;

(* Problem 2 *)

let rec pascal : int * int -> int
= fun (n1, n2) -> 
        if n2 == 0 then 1
        else if n1 == n2 then 1
        else pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2);;

printf "pascal (5, 2) : %d\n" (pascal (5, 2));;
printf "pascal (4, 1) : %d\n" (pascal (4, 1));;
printf "pascal (4, 2) : %d\n" (pascal (4, 2));;
printf "pascal (4, 3) : %d\n" (pascal (4, 3));;
printf "pascal (4, 4) : %d\n" (pascal (4, 4));;
printf "-----------------------------------------\n";;

let rec prime : int -> bool
= fun n -> 
        if n == 1 then true
        else 
                let rec prime_recursive = fun number divisor ->
                        if divisor == 1 then true
                        else if number mod divisor == 0 then false
                        else prime_recursive number (divisor - 1) in
                prime_recursive n (n-1);;
(* Problem 4 *)
let f elem = 
        printf "prime %d: %B\n" elem (prime elem) in
        List.iter f (range 1 10);;

printf "-----------------------------------------\n";;

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
        if a == b then f(b)
        else f(a) + sigma f (a+1) b;;


printf "sigma (fun x -> x) 1 10 : %d\n" (sigma (fun x -> x) 1 10);;
printf "sigma (fun x -> x*x) 1 7 : %d\n" (sigma (fun x -> x*x) 1 7);;
