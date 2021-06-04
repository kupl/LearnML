let comp1 (x : int) (y : int) : int = if x >= y then x else y

let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec __s1 (__s2 : 'c -> 'c -> 'c) (__s3 : 'c list) =
  match __s3 with
  | [] -> raise Failure "list size is 0"
  | [ __s9 ] -> __s9
  | __s10 :: __s11 -> __s2 __s10 (__s1 __s2 __s11)


let rec max (lst : int list) : int = __s1 comp1 lst
