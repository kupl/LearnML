let uniq : 'a list -> 'a list
= fun lst -> 
  let rec is_dup: 'a -> 'a list -> bool
  = fun e l -> match l with 
    | [] -> false
    | hd::tl -> if hd = e then true else is_dup e tl
    
    in
    let rec unique : 'a list -> 'a list -> 'a list
    = fun lst_in lst_out -> match lst_in with
      | [] -> lst_out
      | hd::tl -> if (is_dup hd lst_out) then unique tl lst_out
                  else unique tl (lst_out@[hd])
                  
      in unique lst [];;
      
