let rec checkelem lst a =
  match lst with
  |[] -> []
  |hd::tl -> if hd=a then tl
  else hd::(checkelem tl a);;
  
let rec subapp tbrmved compar =
  match compar with
  |[] -> tbrmved
  |hd::tl -> subapp (checkelem tbrmved hd) tl;;
  
  
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> l2 @ (subapp l1 l2);;