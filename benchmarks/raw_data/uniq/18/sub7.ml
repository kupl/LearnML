let rec list_contains 
  = fun l n -> match l with
    | hd::tl -> if hd = n then true
                else list_contains tl n
    | [] -> false;;
    
let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec consume 
  = fun l1 l2 -> match l1 with
    | hd::tl -> if list_contains l2 hd then consume tl l2
                else consume tl (l2 @ [hd])
    | []-> l2
  in consume lst [];;