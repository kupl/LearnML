let prime : int -> bool
= fun n -> let rec check a b = match b with
  | 1 -> true
  | _ -> (a mod b <> 0) && check a (b-1)
  in match n with
    |0 |1 -> false
    | _ -> check n (n-1);;