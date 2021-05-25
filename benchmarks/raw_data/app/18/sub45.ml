
let rec checkf (n, lst) = match (n, lst) with
  | (_, []) -> true
  | (n, hd::tl) -> if n <> hd then checkf (n, tl) else false;;

let rec amolang lst = match lst with
  | [] -> []
  | hd::tl -> if checkf (hd, tl) then hd::(amolang tl) else amolang tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> amolang (l2@l1);;

app [4;5;6;7] [1;2;3;4];;



(*
let rec length lst = match lst with
  | [] -> 0
  | hd::tl -> 1 + (length tl);;

let rec sortt lst = match lst with
  | a::b::tl -> if a = b then sortt (b::tl) else if a > b then b::(sortt (a::tl)) else a::(sortt (b::tl))
  | _ -> lst;;

let rec sort n lst = if n = 0 then lst else sort (n-1) (sortt lst);;


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> sort (length (l1@l2)) (l1@l2);;

app [345;10;9;8;7;6;5;4;3;2;1] [1;2;3;4;10;9;5;1;1;13;3;3;3;3;3;3;3;3];;
*)