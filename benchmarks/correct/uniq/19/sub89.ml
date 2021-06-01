let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)



    
let rec delete x lst =
    match lst with
      |[] -> []
      |hd::tl ->      
        if hd =x  then delete x tl 
        else hd ::(delete x tl)
        
      
      in 
      
        
  let rec lookfor x lst = 
       match lst with
      |[] -> []
      |hd::tl ->      
        if hd =x  then hd::delete x tl
        else hd::lookfor x tl
        
    in
    
    
    
    
  match lst with
    |[]->[]
    |hd::tl->
      if lookfor hd lst = lst then hd:: uniq tl
      else uniq (lookfor hd lst)
 
  ;;
  
  
uniq [5;6;5;4];;
    