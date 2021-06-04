let bigger (x : int) (y : int) : int = if x > y then x else y

let rec fold (bigger : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> bigger hd (fold bigger tl a)


let rec __s1 (__s2 : 'c -> 'c -> 'c) (__s3 : 'c list) =
  match __s3 with
  | [] -> failwith "Empty List!!"
  | [ __s9 ] -> __s9
  | __s10 :: __s11 -> __s2 __s10 (__s1 __s2 __s11)


let rec max (lst : int list) : int = __s1 bigger lst
