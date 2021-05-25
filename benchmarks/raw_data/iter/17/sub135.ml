(* 2015 - 14718 Giyeon Kim HW 1 *)

(* Exercise 3 *)
let rec iter (n, f) x =
    if (n <= 0) then x
    else iter (n - 1, f) (f x)

