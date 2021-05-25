
  let rec app l1 l2 = 
  match l1 with
    | [] -> l2
    | hd::tl -> hd::(app tl l2);;
