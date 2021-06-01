let rec check_dup a lst =
        match lst with
          | [] -> false
          | hd::tl -> a = hd || check_dup a tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec unique reslst lst =
  match lst with
    | [] -> reslst
    | hd::tl -> if check_dup hd reslst then unique reslst tl else unique (reslst@[hd]) tl
    in unique [] lst;;
    
uniq [5;6;5;4];;