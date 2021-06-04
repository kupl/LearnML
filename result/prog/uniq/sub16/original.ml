let uniq (lst : 'a list) : 'b list =
  let rec tail_uniq (a : 'a list) (lst : 'a list) : 'a list =
    match lst with
    | [] -> a
    | hd :: tl -> tail_uniq (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tail_uniq [] lst


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
