let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec elechk n lst =
  match lst with
  |[] -> false
  |h::t -> if h = n then true
           else elechk n t in
  let rec remove n lst =
  match lst with
  |[] -> []
  |h::t -> if n = h then remove n t
           else h::remove n t in
  match lst with
  |[] -> []
  |h::t -> if elechk h t then h::uniq(remove h t)
           else h::uniq t;;

let a = uniq [5;6;5;4];;
let b = uniq [6;9;2;5;5;9;2;4];;
let c = uniq [6;6;8;9;4;2;5;1;5;2;3];;