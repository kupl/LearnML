let prime : int -> bool
= fun n ->
  match n with
    | 0 -> false
    | 1 -> false
    | _ -> let x = (n -1) in
    let rec checkprime x n =
      if x > 1 then
      match n mod x with
      | 0 -> false
      | _ -> checkprime (x-1) n
      else true
      in checkprime x n;;

prime 2;;
prime 3;;
prime 4;;
prime 17;;