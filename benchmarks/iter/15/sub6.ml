(*
    PL 1-3
    2008-11609 박성원
*)

let rec iter (n, f) =
  if n = 0
    then fun x -> x
    else fun x -> f (iter (n-1, f) x)
;;
