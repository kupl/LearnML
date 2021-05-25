let rec delete a lst = 
  match lst with
    |[] -> []
    |hd::tl -> if hd = a then delete a tl else hd::(delete a tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  |[] -> []
  |hd::tl -> hd::(uniq (delete hd tl));;
  
uniq [5;6;5;4];;