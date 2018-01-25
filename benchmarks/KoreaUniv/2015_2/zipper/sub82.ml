let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a with
    [] -> if b = [] then [] else zipper(b, [])
   |hd::tl -> hd::zipper(b,tl)
