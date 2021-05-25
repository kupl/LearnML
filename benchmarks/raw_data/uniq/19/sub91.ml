let rec exist : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
    |[] -> false
    |hd::tl -> if hd = a then true else exist a tl;;

let rec del : 'a -> 'a list -> 'a list  
=fun a lst ->
  match lst with
    |[] -> []
    |hd::tl -> 
      if hd = a then del a tl
      else hd::(del a tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    |[] -> []
    |hd::tl -> hd::(uniq (del hd tl));;