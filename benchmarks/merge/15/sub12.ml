let rec merge (l1,l2) = match l1 with
  | [] -> l2
  | l1h::l1t -> 
      if l1h > (match l2 with
              | [] -> l1h
              | l2h::l2t -> l2h)
      then l1h :: merge (l1t,l2)
      else match l2 with
           | [] -> l1
           | l2h::l2t -> l2h :: merge (l1,l2t)
