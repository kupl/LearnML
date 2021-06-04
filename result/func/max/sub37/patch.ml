let rec __s1 (__s2 : 'a -> 'a -> 'a) (__s3 : 'a list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | [ __s10 ] -> __s10
  | __s11 :: __s12 -> __s2 __s11 (__s1 __s2 __s12 __s4)


let rec max (lst : int list) : int =
  let rec fold (action : int -> int -> int) (l : int list) (a : int) : int =
    match l with [] -> a | hd :: tl -> action hd (fold action tl a)
  in

  let max (a : int) (b : int) : int = if a > b then a else b in
  __s1 max lst 0
