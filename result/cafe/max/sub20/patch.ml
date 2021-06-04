let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | [ __s9 ] -> __s9 | hd :: tl -> f hd (fold f tl a)


let rec max (lst : int list) : int =
  let large (a : int) (b : int) : int = if a > b then a else b in
  fold large lst 0
