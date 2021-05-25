let rec  checkzero p q=
  match q with
    | 1->true
    | _ -> (p mod q <> 0) && checkzero p (q-1);;

let prime : int -> bool
= fun n -> match n with
    | 0 | 1 -> false
    | _ -> checkzero n (n-1) ;;
    

prime 2;;
prime 4;;