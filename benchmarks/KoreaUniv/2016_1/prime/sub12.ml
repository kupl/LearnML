(* Problem 3 *)

let rec mark x y =
    if x mod y = 0 then 0
    else 1 ;;

let rec sigma mark a b =
    if b = 1 then 0
   else mark a b + (sigma mark a (b-1)) ;;

let prime x =
    if x = 1 then false
    else if sigma mark x x = (x-2) then true else false ;;

 (* TODO *)
