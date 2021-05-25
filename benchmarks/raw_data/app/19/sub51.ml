let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec cmp lst n = 
    match lst with
      | [] -> false
      | hd::tl -> if hd=n then true else cmp tl n in
  match l1 with
    | [] -> l2
    | hd::tl -> if cmp l2 hd then app tl l2 else app tl (l2@[hd]);;
    
app [4;5;6;7] [1;2;3;4];;