
(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 2: sigma
 * Written by Dongho Kang 
 *)

let rec sigma: int * int * ( int -> int ) -> int = fun (a, b, f_n) ->
    (* sigma f_n: n is a to b *) 
    
    if a == b then f_n a (* base case *)
    else if a > b then 0 (* exception: a should not be bigger than b *)
    else f_n a + sigma (a+1, b, f_n) (* recursive step *)
;;
