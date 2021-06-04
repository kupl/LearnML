let rec __s1 (__s2 : 'a -> 'a -> 'a) (__s3 : 'a list) (__s4 : int) =
  match __s3 with
  | [] -> __s4
  | [ __s9 ] -> __s9
  | __s10 :: __s11 -> __s2 __s10 (__s1 __s2 __s11 __s4)


let rec max (lst : int list) : int =
  let rec fold (f : int -> int -> int) (l : int list) (a : int) : int =
    match l with [] -> a | hd :: tl -> f hd (fold f tl a)
  in
  __s1
    (fun (__s7 : int) (__s8 : int) -> if __s8 > __s7 then __s8 else __s7)
    lst 0
