let rec max : int list -> int =
 fun lst ->
  let rec fold action l a =
    match l with [] -> a | hd :: tl -> action hd (fold action tl a)
  in

  let max a b = if a > b then a else b in
  fold max lst 0
