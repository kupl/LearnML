let uniq : 'a list -> 'a list =
 fun lst ->
  let rec tu a l =
    match l with
    | [] -> a
    | hd :: tl -> tu (hd :: a) (List.filter (fun x -> x != hd) tl)
  in
  tu [] lst


let _ = uniq [ 5; 4; 3; 5 ]
