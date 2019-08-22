(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 3: check
 * Written by Dongho Kang 
 *)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string
;;

let rec check: lambda -> bool = fun m -> 
    let rec check_sub: lambda * var list -> bool = fun (m, l) ->
        match m with 
        | V n             -> (List.mem n l)
        | P (n, m_n)         -> check_sub (m_n, n::l)
        | C (m_1, m_2)    -> check_sub (m_1, l) && check_sub (m_2, l)
    in

    check_sub (m, [])
;;

