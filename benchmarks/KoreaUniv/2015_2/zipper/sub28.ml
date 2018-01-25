let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with
| [] -> b
| hd::tl -> hd::zipper(b,tl)
