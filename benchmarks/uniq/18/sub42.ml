let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    []-> []
    |hd::tl -> if (a = hd) then true else (uniq a tl);;

