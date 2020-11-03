let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec isexisting : 'a list -> 'a -> bool
  = fun lst e ->
    match lst with 
      | hd::tl -> if hd = e then true else isexisting tl e
      | [] -> false
  in let rec makeset : 'a list -> 'a list -> 'a list
  = fun lst result ->
    match lst with 
      | hd::tl -> if isexisting result hd then makeset tl result  else makeset tl (result @ [hd])
      | [] -> result
  in makeset lst [];;