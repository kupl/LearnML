let who_is_G (x : int) (y : int) : int = if x > y then x else y

let rec fold (f : 'a -> int -> int) (l : 'a list) (a : int) : int =
  match l with [] -> 1 | hd :: tl -> f hd (fold f tl a)


let rec __s1 (__s2 : 'b -> 'b -> 'b) (__s3 : 'b list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | [ __s10 ] -> __s10
  | __s11 :: __s12 -> __s2 __s11 (__s1 __s2 __s12 __s4)


let max (lst : int list) : int = __s1 who_is_G lst 0
