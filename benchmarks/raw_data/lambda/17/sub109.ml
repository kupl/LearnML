(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 4 *)
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
     and var = string

let check: lambda -> bool = fun ilambda ->
    let rec checkInner: lambda -> var list -> bool = fun ilambda ilist ->
	    match ilambda with
        | V lvar -> List.mem lvar ilist
        | P (lvar, rlambda) -> checkInner rlambda (lvar::ilist)
        | C (llambda, rlambda) -> checkInner llambda ilist && checkInner rlambda ilist
    in checkInner ilambda []

