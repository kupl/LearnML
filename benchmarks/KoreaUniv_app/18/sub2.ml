let rec fastrev : 'a list -> 'a list = fun lst -> 
  let rec rev a accum =
    match a with
      |[] -> accum
      |hd::tl -> rev tl ([hd] @ accum)
  in rev lst [];;

let rec search : 'a list -> 'a -> bool = fun l1 e ->
  match l1 with
    |[] -> false
    |hd::tl -> if hd = e then true else search tl e;;

let rec delete : 'a list -> 'a list = fun lst ->
  match lst with
    |[] -> []
    |hd::tl -> if search tl hd then delete tl else [hd] @ delete tl;;

let rec app : 'a list -> 'a list -> 'a list = fun l1 l2 ->
  match l1 with
    |[] -> l2
    |hd::tl -> if (search l2 hd) then app tl l2 else app tl (l2 @ [hd]);;
    
delete (app [4;5;4;6;7] [1;2;3;4;5;6;4]);;
fastrev (delete (fastrev [1;2;3;4;5;6;4]));;