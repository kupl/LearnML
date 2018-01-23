let rec merge args =
    match args with
    | (l1, []) -> l1
    | ([], l2) -> l2
    | (h1::t1, h2::t2)-> 
      if (h1=h2) then h1::(merge (t1,t2)) 
      else if (h1<h2) then h2::(merge (h1::t1,t2))
      else raise Exit
;;
