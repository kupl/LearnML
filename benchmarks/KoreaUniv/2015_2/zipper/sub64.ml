let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match (a,b) with
  | ([], []) -> []
  | ([], _) -> b
  | (_, []) -> a
  | _ -> (List.hd a)::(List.hd b)::zipper((List.tl a), (List.tl b)) 
