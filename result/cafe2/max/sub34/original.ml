let rec max (lst : int list) : int =
  let rec fold (f : int -> int -> int) (l : int list) (a : int) : int =
    match l with [] -> a | hd :: tl -> f hd (fold f tl a)
  in
  fold (fun (x : int) (y : int) -> if x > y then x else y) lst 0
