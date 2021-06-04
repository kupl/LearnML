let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> fold f tl (f hd a)


let rec max (l : int list) : int =
  let a : int = -1073741824 in
  fold (fun (x : int) (y : int) -> if x > y then x else y) l a
