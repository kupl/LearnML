let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec max (lst : int list) : int =
  fold (fun (a : int) (b : int) -> if a > b then a else b) lst 0
