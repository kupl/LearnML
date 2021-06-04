let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> f hd (fold f tl a)


let rec max (lst : int list) : int =
  fold
    (fun (__s7 : int) (__s8 : int) -> if __s8 >= __s7 then __s8 else __s7)
    lst min_int
