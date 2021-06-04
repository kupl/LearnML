let rec dupl a (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> if hd = a then true else dupl a tl


let rec uniq (lst : 'b list) : 'b list =
  match List.rev lst with
  | [] -> []
  | hd :: tl ->
      if dupl hd tl then uniq (List.rev tl) else uniq (List.rev tl) @ [ hd ]


let (_ : int list) = uniq [ 5; 6; 5; 4 ]

let (_ : int list) = uniq [ 3; 6; 8; 4; 6; 3; 7; 9; 8 ]
