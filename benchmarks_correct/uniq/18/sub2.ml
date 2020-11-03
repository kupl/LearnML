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
  
let rec uniq : 'a list -> 'a list = fun lst ->
  let flst = fastrev lst in
  let dflst = delete flst in
  fastrev dflst;;