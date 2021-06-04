let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | [] -> []
  | h :: t -> if List.exists (fun x -> x = h) t then uniq t else h :: uniq t
