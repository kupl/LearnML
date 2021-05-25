let  app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
 
  
  let rec delete x lst =
    match lst with
      |[] -> []
      |hd::tl ->      
        if hd =x  then delete x tl 
        else hd ::(delete x tl)
        
      
      in 
      
        
  let rec lookfor x lst = 
       match lst with
      |[] -> false
      |hd::tl ->      
        if hd =x  then true
        else lookfor x tl
        
  in
  
let rec comp l1 l2 = 
  match l2 with
    |[]-> l1
    |hd::tl ->
      if lookfor hd l1 = false then comp  l1  tl
      else (comp (delete hd l1) ) l2
  
    
    in
    
  let rec append l1 l2 = 
  match l1 with
    | []-> l2
    | hd::tl -> 
      hd::(append tl l2)
    
    in
    
    




 append l2 (comp l1 l2)




;;
    
    

app [4;5;6;7][1;2;3;4];;  

        

        
   
