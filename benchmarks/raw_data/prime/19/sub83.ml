let prime : int -> bool
= fun n ->
  let rec pr n a =
    match a with
      | 0 -> false
      | 1 -> true
      | d -> if n mod d = 0 then false else pr n (d-1)
  in pr n (n-1) ;;
