let comp1 (x : int) (y : int) : int = if x >= y then x else y

let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec max (lst : int list) : int = fold comp1 lst (-100000000)
