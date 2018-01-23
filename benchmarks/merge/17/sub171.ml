let merge ((x:int list),(y:int list)) : int list =
  let rec merge' acc l l' =
    match l, l' with
    | [], [] -> acc
    | [], _ -> List.append acc l'
    | _, [] -> List.append acc l
    | h::t, h'::t' ->
      if h > h' then h:: (merge' acc t l' )
      else  h':: (merge' acc l t')
  in
  merge' [] x y
    
