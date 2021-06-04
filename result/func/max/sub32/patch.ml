let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec __s1 (__s2 : 'c -> 'c -> 'c) (__s3 : 'c list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | [ __s9 ] -> __s9
  | __s10 :: __s11 -> __s2 __s10 (__s1 __s2 __s11 __s4)


let rec max (lst : int list) : int =
  __s1
    (fun (__s7 : int) (__s8 : int) -> if __s8 > __s7 then __s8 else __s7)
    lst 0
