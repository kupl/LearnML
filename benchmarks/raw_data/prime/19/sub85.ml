let prime : int -> bool
= fun n ->
  let rec calc : int -> int -> bool
  = fun n i ->
    match i with
    | 0 -> false
    | 1 -> true
    | i' -> if n mod i' = 0 then false else calc n (i'-1)
  in calc n (n-1)
;;

(*
prime 1;;
prime 3;;
prime 2;;
prime 4;;
prime 17;;
prime 21;;
*)
