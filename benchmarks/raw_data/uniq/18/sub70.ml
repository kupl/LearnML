let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  |[]->[]
  |hd::tl->let rec remove a l =match l with
    |[]->[]
    |h::t->if h=a then t else h::(remove a t) 
    in hd::remove hd (uniq tl);;
    
uniq [5;6;5;4];;
