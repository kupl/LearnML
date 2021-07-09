let rec check_dup e (l : 'a list) : bool =
  match l with
  | [] -> false
  | hd :: tl -> if e = hd then true else check_dup e tl


let rec uniq (l : 'b list) : 'b list =
  match List.rev l with
  | [] -> l
  | hd :: tl ->
      if check_dup hd tl then uniq (List.rev tl)
      else uniq (List.rev tl) @ [ hd ]
