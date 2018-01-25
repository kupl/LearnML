let change : int list -> int -> int 
  = fun coins amount -> let rec leng lst = match lst with | [] -> 0 | hd::tl -> 1+leng tl
  in let rec find_ele lst n = match lst with | [] -> 0 | hd::tl -> if n = 0 then hd else find_ele tl (n-1)
  in let rec ch coins amount idx = if amount = 0 then 1 else if amount < 0 || idx >= leng coins then 0 else (ch coins (amount-(find_ele coins idx)) idx) + (ch coins amount (idx+1)) in ch coins amount 0