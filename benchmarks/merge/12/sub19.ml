(* Ex1 *)
let rec merge (lst1, lst2) =
   match (lst1, lst2) with 
   | ([], _) -> lst2
   | (_, []) -> lst1
   | (h1::t1, h2::t2) -> 
      if h1 < h2 then h2::merge (lst1, t2)
	  else h1::merge (t1, lst2)