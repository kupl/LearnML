let rec uniq lst =(*: 'a list -> 'a list*)
let rec match_uniq l n =
  match l with
    | [] -> []
    | hd::tl -> if hd <> n then hd::(match_uniq tl n) else match_uniq tl n in
  match lst with
  | [] -> []
  | hd::[] -> hd::[]
  | hd::(h::t) ->
    if hd = h then uniq (h::t) 
    else hd::(match_uniq (uniq (h::t)) hd)
    ;;

