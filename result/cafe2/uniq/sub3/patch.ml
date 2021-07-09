let rec checkDuplicate a (lst : 'a list) : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if a = hd then true else checkDuplicate a tl


let rec uniq (lst : 'c list) : 'b list =
  match List.rev lst with
  | [] -> []
  | hd :: tl ->
      let temp = hd in
      if checkDuplicate temp tl then uniq (List.rev tl)
      else uniq (List.rev tl) @ [ hd ]
