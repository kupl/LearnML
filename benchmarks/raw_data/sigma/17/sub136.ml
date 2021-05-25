(* 2015 - 14718 Giyeon Kim HW 1 *)

(* Exercise 2 *)
let rec sigma (a, b, f) =
    if (a > b) then 0
    else if (a == b) then f a
    else f a + sigma (a + 1, b, f)

