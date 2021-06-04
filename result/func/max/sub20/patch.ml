let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec __s1 (__s2 : 'c -> 'c -> 'c) (__s3 : 'c list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | [ __s10 ] -> __s10
  | __s11 :: __s12 -> __s2 __s11 (__s1 __s2 __s12 __s4)


let rec max (lst : int list) : int =
  let large (a : int) (b : int) : int = if a > b then a else b in
  __s1 large lst 0
