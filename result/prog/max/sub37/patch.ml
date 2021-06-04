let rec __s1 (__s2 : 'a -> 'a -> 'a) (__s3 : 'a list) =
  match __s3 with
  | [] -> failwith "Empty List!!"
  | [ __s9 ] -> __s9
  | __s10 :: __s11 -> __s2 __s10 (__s1 __s2 __s11)


let rec max (lst : int list) : int =
  let rec fold (action : int -> int -> int) (l : int list) (a : int) : int =
    match l with [] -> a | hd :: tl -> action hd (fold action tl a)
  in

  let max (a : int) (b : int) : int = if a > b then a else b in
  __s1 max lst
