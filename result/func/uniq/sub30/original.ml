let uniq (lst : 'a list) : 'b list =
  let rec tu (a : 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> a
    | hd :: tl -> tu (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tu [] lst


let (_ : int list) = uniq [ 5; 4; 3; 5 ]
