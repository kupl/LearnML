let lst2int : int list -> int
= fun lst ->
  let rec lst2intc : int list -> int -> int
  = fun lst1 n ->
    match lst1 with
    [] -> n
    |h::t -> lst2intc t (n * 10 + h)
    in
  lst2intc lst 0;;
