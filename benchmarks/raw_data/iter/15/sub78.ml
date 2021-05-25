(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 1, Exercise 3 *)

let rec makefunc n f result =
    if n == 0 then
        result
    else
        makefunc (n - 1) f (f result)

let iter (n, f) = fun x -> (makefunc n f x)
