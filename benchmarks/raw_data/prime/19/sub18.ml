let prime : int -> bool
= fun n ->
  match n with
    0 | 1 -> false
    |_ ->
      let a = 2 in
      let rec p n a =
        if n = a then true
        else
          match (n mod a) with
            0 -> false
            |_-> p n (a+1)
      in
      p n a
;;