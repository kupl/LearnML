let rec delete lst a= 
  match lst with 
    |[]->lst
    |h::t-> if h=a then (delete t a) else h::(delete t a);;


let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with 
    |[] -> lst
    |h::t-> h::(uniq (delete t h));;
    
    uniq [5;6;5;4];;
    uniq [1;2;3;5;6;6;3;2];;