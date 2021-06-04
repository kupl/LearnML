let rec find item (l : 'a list) : bool =
  match l with [] -> false | h :: t -> h = item || find item t


let rec uniq (lst : 'b list) : 'c list =
  let rec uniq' (acc : 'b list) (lst : 'b list) : 'b list =
    match lst with
    | [] -> acc
    | hd :: tl -> if find hd tl then uniq' acc tl else uniq' (acc @ [ hd ]) tl
  in
  uniq' [] lst


let rec app (l1 : 'd list) (l2 : 'd list) : 'c list =
  match l1 with
  | [] -> l2
  | hd :: tl -> if find hd l2 then app tl l2 else app tl (l2 @ [ hd ])


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
