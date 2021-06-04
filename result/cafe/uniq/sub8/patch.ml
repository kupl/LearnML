let rec thereis a (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> if a = hd then true else thereis a tl


let rec uniq (lst : 'c list) : 'b list =
  match List.rev lst with
  | [] -> []
  | hd :: tl ->
      if thereis hd tl then uniq (List.rev tl) else uniq (List.rev tl) @ [ hd ]


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
