(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #3 *)
let rec iter (n, f) =
        if n = 0 then (fun x -> x)
        else (fun x -> f (iter (n-1, f) x))
