let rec dup a (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> if a = hd then true else dup a tl


let rec uniq (lst : 'c list) : 'b list =
  match List.rev lst with
  | [] -> []
  | hd :: tl ->
      if dup hd tl then uniq (List.rev tl) else uniq (List.rev tl) @ [ hd ]


let (_ : int list) = uniq [ 1; 2; 3; 3; 4; 4 ]
