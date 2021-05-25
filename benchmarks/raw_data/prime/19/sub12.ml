let rec primecheck n a =
  match a with 
    | 1 -> true
    | a -> if n mod a = 0 then false else primecheck n (a-1)
    ;;
 let prime : int -> bool
  = fun n -> (*TODO*)
    primecheck n (n-1)
;;

