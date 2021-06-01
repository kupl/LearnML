let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
let rec remove l a = 
  match l with
    |[] -> []
    |hd::tl -> if hd = a then remove tl a else hd::(remove tl a) in
  match lst with
    |[] -> []
    |hd::tl -> (hd::uniq(remove tl hd));;