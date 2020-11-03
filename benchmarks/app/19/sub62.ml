let rec checkelem list a =
  match list with
  |[] -> []
  |hd::tl -> if hd=a then tl
  else hd::(checkelem tl a);;
  
let rec subapp tbrmved compar =
  match compar with
  |[] -> tbrmved
  |hd::tl -> subapp (checkelem tbrmved hd) tl;;
  
  
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> l2 @ (subapp l1 l2);;

app [4;5;6;7] [1;2;3;4];;
