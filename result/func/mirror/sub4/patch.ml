type btree = Empty | Node of (int * btree * btree)

let rec __s3 (__s4 : btree) : btree =
  match __s4 with
  | Node (__s5, __s6, __s7) ->
      if __s6 = Empty && __s7 = Empty then Node (__s5, __s7, __s6)
      else Node (__s5, __s3 __s7, __s3 __s6)
  | Empty -> Empty


let rec mirror (t : btree) : btree = __s3 t
