let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec isexisting : 'a list -> 'a -> bool
  = fun lst e ->
    match lst with
      | hd::tl -> if e = hd then true else isexisting tl e
      | [] -> false
  in let rec makeset : 'a list -> 'a list -> 'a list
  = fun lst1 lst2 -> 
    match lst1 with 
      | hd::tl -> 
        if isexisting lst2 hd then makeset tl lst2
        else makeset tl (lst2 @ [hd]) 
      | [] -> lst2
  in makeset (makeset l1 l2) [];;
  
app [1;1;1;1;1] [1;1;1;1;1];;