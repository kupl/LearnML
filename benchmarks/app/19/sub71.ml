let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  let rec infun x lst = match lst with
    |h::t -> if x = h then true else infun x t
    |[] -> false
  
  in let rec ininfun l1 l2 = match l1 with
    |h::t -> if infun h l2 then ininfun t l2 else ininfun t (l2 @ [h])
    |[] -> l2
  
  in ininfun l1 l2;;
    
app [4;5;6;7] [1;2;3;4];;