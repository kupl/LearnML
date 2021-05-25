let rec duplicate x lst =
    match lst with
      | [] -> false
      | hd::tl -> if x = hd then true else duplicate x tl 

let rec remove a lst=
  match lst with
    | []->[]
    | hd :: tl -> if hd = a then remove hd tl else hd:: remove a tl;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    | [] -> []
    | hd::tl -> if duplicate hd tl then hd::remove hd (uniq tl) else hd::(uniq tl);;
    
    
   
  uniq [1;2;3;4;1];;
  uniq [5;6;5;4];;
  