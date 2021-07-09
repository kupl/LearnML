let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec max (lst : int list) : int =
  fold (fun (x : int) (y : int) -> if x > y then x else y) lst (-9999999)
