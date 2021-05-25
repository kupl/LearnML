(*Cedric Brown, prime *)

let rec testnum a b = match b with
  | 1 -> true 
  | _ -> (a mod b <> 0) && testnum a (b -1);;

let rec prime : int -> bool
= fun n -> match n with
  |0|1 -> false
  |_ -> testnum n (n-1)
;;
(*Use expression below, **prime int;;** *)
prime 7;;
prime 1;;