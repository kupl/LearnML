let rec filter pred lst =  (* TODO *)
match lst with
| [] -> []
| hd::tl when (pred hd) = true -> hd::(filter pred tl)
| hd::tl -> (filter pred tl) 
