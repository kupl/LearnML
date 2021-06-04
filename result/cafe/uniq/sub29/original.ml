let uniq (lst : 'a list) : 'b list =
  let rec chk x (lst : 'a list) : bool =
    match lst with h :: t -> if x = h then true else chk x t | [] -> false
  in

  let rec infun (lst : 'a list) : 'a list =
    match lst with
    | h :: t -> if chk h t then infun t else h :: infun t
    | [] -> []
  in
  infun lst


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
