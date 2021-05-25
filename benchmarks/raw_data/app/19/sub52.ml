let rec check_dup e l =
  match l with 
    | [] -> false
    | hd::tl -> if e=hd then true else check_dup e tl ;;


let rec app fl sl =
  match fl with 
    | [] -> sl
    | hd::tl -> if (check_dup hd sl) then (app tl sl) else (app tl (hd::sl)) ;;
    