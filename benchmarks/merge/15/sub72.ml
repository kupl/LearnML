let merge (ls1, ls2) =
    let rec aux (ls1, ls2) ret =
        if ls1 = [] then (List.rev ret)@ls2
        else if ls2 = [] then (List.rev ret)@ls1
  else
      let a::subls1 = ls1 in
      let b::subls2 = ls2 in
      if a >= b then aux (subls1, ls2) (a::ret)
      else aux (ls1, subls2) (b::ret)
      in
  if ls1 = [] then ls2
  else if ls2 = [] then ls1
  else aux (ls1, ls2) [];;
