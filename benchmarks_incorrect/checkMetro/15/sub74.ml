(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 2, Exercise 3 *)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

(* helper function isStringExist *)
let isStringExist e l =
        let x = List.find (fun x -> (x = e)) l in
        true

(* check *)
let check lambda =
    let rec checkWithArea areaList lambda =
        match lambda with
        | V var -> isStringExist var areaList
        | P (var, lambda) -> checkWithArea (areaList@[var]) lambda
        | C (m1, m2) -> (checkWithArea areaList m1) &&
                              (checkWithArea areaList m2)
    in
    checkWithArea [] lambda
