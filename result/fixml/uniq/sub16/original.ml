let uniq : 'a list -> 'a list =
 fun lst ->
  let rec tail_uniq a lst =
    match lst with
    | [] -> a
    | hd :: tl -> tail_uniq (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tail_uniq [] lst


let _ = uniq [ 5; 6; 5; 4 ]
