let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | [ __s9 ] -> __s9 | hd :: tl -> f hd (fold f tl a)


let max (lst : int list) : int =
  fold (fun (x : int) (y : int) -> if x > y then x else y) lst 0
