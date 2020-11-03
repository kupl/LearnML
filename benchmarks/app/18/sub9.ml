let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec is_dup : 'a -> 'a list -> bool
  = fun e l -> match l with
    |[] -> false
    |hd::tl -> if hd=e then true else is_dup e tl

    in
    let rec append : 'a list -> 'a list -> 'a list -> 'a list
    = fun l1 l2 l_out -> match l1 with
      | [] -> l2@l_out
      | hd::tl -> if (is_dup hd (l2@l_out)) then append tl l2 l_out
                  else append tl l2 (l_out@[hd])
                  
      in append l1 l2 [];;
