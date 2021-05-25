let rec div n m = if n <= m then true else
  if n mod 2 = 0 then false else
    if n mod m = 0 then false else div n (m+2) ;;


let prime : int -> bool
= fun n -> if n < 2 then false else div n 3 ;;

prime 2 ;;
prime 3 ;;
prime 4 ;;
prime 17 ;;
prime 2357 ;;