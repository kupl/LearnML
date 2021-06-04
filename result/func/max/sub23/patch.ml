let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec __s3 (__s4 : int -> int -> int) (__s5 : int list) : int =
  match __s5 with
  | [ __s8 ] -> __s8
  | __s9 :: __s10 -> __s4 __s9 (__s3 __s4 __s10)


let max (lst : int list) : int =
  __s3
    (fun (__s6 : int) (__s7 : int) -> if __s7 >= __s6 then __s7 else __s6)
    lst
