let rec dupck : 'a list -> 'a -> bool
= fun lst n -> match lst with
  | [] -> false
  | hd::tl -> if hd=n then true else dupck tl n;;

let rec uniq : 'a list -> 'a list
= fun lst -> let rec unqck : 'a list -> 'a list -> 'a list
= fun arr ans -> match arr with
  | [] -> ans
  | hd::tl -> if dupck ans hd = false then unqck tl (ans@[hd]) else unqck tl ans
  in unqck lst [];;
  
uniq [5;6;5;4];;