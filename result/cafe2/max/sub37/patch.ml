let rec max (lst : int list) : int =
  let rec fold (action : int -> int -> int) (l : int list) (a : int) : int =
    match l with
    | [] -> a
    | [ __s9 ] -> __s9
    | hd :: tl -> action hd (fold action tl a)
  in

  let max (a : int) (b : int) : int = if a > b then a else b in
  fold max lst 0
