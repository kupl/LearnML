let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> let l = [] in 
if pred(hd) = true then l @ [hd] @ (filter pred tl)
else l @ (filter pred tl)
