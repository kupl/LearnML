let who_is_G (x : int) (y : int) : int = if x > y then x else y

let rec fold (f : 'a -> int -> int) (l : 'a list) (a : int) : int =
  match l with [] -> 1 | hd :: tl -> f hd (fold f tl a)


let max (lst : int list) : int =
  match lst with __s6 :: __s7 -> List.fold_left who_is_G __s6 __s7
