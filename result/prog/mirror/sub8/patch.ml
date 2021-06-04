type btree = Empty | Node of (int * btree * btree)

let rec __s3 (__s4 : btree) : btree =
  match __s4 with
  | Empty -> __s4
  | Node (__s5, __s6, __s7) -> Node (__s5, __s3 __s7, __s3 __s6)


let rec mirror (f : btree) : btree = __s3 f
