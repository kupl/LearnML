let rec index x v = 
  match x with
    | [] -> -1
    | h::t -> 
      if h == v then 0
      else 1+ (index t v)