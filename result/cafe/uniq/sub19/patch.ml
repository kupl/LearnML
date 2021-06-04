let rec dupl a (lst : 'a list) : bool =
  match lst with [] -> false | hd :: tl -> if a = hd then true else dupl a tl


let rec uniq (lst : 'c list) : 'b list =
  match List.rev lst with
  | [] -> []
  | hd :: tl ->
      if dupl hd tl then uniq (List.rev tl) else uniq (List.rev tl) @ [ hd ]
