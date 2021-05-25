let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst -> match lst with
| [] -> []
| hd::tl -> if (pred hd) then [hd;]@(filter pred tl) else (filter pred tl)
