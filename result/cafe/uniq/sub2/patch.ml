let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | [] -> []
  | h :: t -> h :: uniq (List.filter (fun __s5 -> __s5 != h) t)
