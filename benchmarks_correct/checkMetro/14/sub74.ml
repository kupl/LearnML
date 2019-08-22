(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 23, 2014
 *)

(* Exercise 1 : CheckMetroMap *)
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check: lambda -> bool = fun m ->
  let rec checkSub: var list -> lambda -> bool = fun nl m ->
    match m with
    | V var             -> (List.mem var nl)
    | P (var, lambda)       -> (checkSub (var::nl) lambda)
    | C (lambda1, lambda2) -> (checkSub nl lambda1) && (checkSub nl lambda2)
  in
  (checkSub [] m)