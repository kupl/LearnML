let uniq : 'a list -> 'a list =
 fun lst ->
  let rec chk x lst =
    match lst with h :: t -> if x = h then true else chk x t | [] -> false
  in

  let rec infun lst =
    match lst with
    | h :: t -> if chk h t then infun t else h :: infun t
    | [] -> []
  in
  infun lst


let _ = uniq [ 5; 6; 5; 4 ]
