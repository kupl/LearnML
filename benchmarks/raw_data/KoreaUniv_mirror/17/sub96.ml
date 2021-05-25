(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec f t = match t with
                          | Node(n,a,b) ->  if a = Empty && b = Empty then Node(n,b,a) else Node(n,f b,f a)
                          | Empty -> Empty
  in f t 