
(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 2: sigma
 * Written by Dongho Kang 
 *)

let rec sigma f_n a b  =
    (* sigma f_n: n is a to b *) 
    
    if a == b then f_n a (* base case *)
    else if a > b then 0 (* exception: a should not be bigger than b *)
    else f_n a + sigma f_n (a+1) b (* recursive step *)
;;
