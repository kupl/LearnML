let rec check_dup e l =
  match l with 
    | [] -> false
    | hd::tl -> if e=hd then true else check_dup e tl ;;

let rec uniq l = 
  match l with 
    | [] -> l
    | hd::tl -> if (check_dup hd tl) then uniq tl else hd::(uniq tl)  ;;
    