let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    []-> l2
    |h::t -> if (List.exists (fun x-> x=h) l2) then app t l2 else app t (l2@[h]);;
    
    
  
