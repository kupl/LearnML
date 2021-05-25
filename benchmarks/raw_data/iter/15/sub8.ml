(* SNU Programming Language Fall 2015
 * Homework 1 
 * Exercise 3: iter
 * Written by Dongho Kang 
 *)
let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
(* iter (n, f(x)) *)

    let f_i = fun x -> x in (* identity function *)

    if n == 0 then f_i (* base case: return identity ftn when n=0 *)
    else if n < 0 then f_i
    else (* recursive step *)
        let f_n x = f (iter(n-1, f) x) in 
        f_n
;;
