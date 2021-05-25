let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    []->[]
    |h::t-> if List.exists (fun x->x=h) t then uniq t else h::(uniq t);;
    
   

    
