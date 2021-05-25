let prime : int -> bool
= fun n -> (*TODO*)
  let rec q_is_zero p =
    match p with
      | (_,1)  -> true
      | (a,b) -> if a mod b = 0 then false else q_is_zero (a, b-1) in
  match n with
    | 1 -> false
    | x -> q_is_zero (x, x-1);;

prime 2;;
prime 3;;
prime 4;;
prime 17;;
