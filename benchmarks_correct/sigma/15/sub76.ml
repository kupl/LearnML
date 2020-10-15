(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 1, Exercise 2 *)

let rec iter_f a b f result =
    if a <= b then
        iter_f (a + 1) b f (result + (f a))
    else
        result

let sigma f a b  = iter_f a b f 0
