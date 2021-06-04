let who_is_G (x : int) (y : int) : int = if x > y then x else y

let rec fold (f : 'a -> int -> int) (l : 'a list) (a : int) : int =
  match l with [] -> 1 | hd :: tl -> f hd (fold f tl a)


let max (lst : int list) : int = fold who_is_G lst 1
