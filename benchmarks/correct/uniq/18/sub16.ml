let rec chk = fun lst a -> match lst with
  [] -> true
  |hd::tl -> if (hd = a) then false else chk tl a;; 

let rec del = fun lst a -> match lst with
  [] -> []
  |hd::tl -> if (hd = a) then del tl a else hd :: del tl a;; 

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  [] -> []
  |hd :: tl -> if (chk tl hd) then hd :: uniq tl else hd :: uniq (del tl hd);;