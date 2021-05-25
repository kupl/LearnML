let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
match l1 with 
  | [] -> l2
  | hd::tl -> if (isfind l2 hd) then (app tl (l2@[hd])) else (app tl l2)
    
and isfind = fun l h ->
match l with 
  | [] -> true
  | hd::tl -> if hd = h then false else isfind tl h;; 
  
