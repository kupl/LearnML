let rec division = fun n d -> if d =1 then false else if n mod d = 0 then true else division n (d-1) ;;
let rec prime : int -> bool
= fun n -> if division n (n-1) then false else true 
