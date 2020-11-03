let rec find_duplicate
= fun e l ->
  match l with
    | [] -> false
    | h::t -> if h==e then true else find_duplicate e t;;

let reverse lst =
    let rec reverse_accu accu lst = 
      match lst with
      | [] -> accu
      | h::t -> reverse_accu (h::accu) t 
    in reverse_accu [] lst;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec uniq_accu lst accu=
    match lst with
      |[] -> accu
      |hd::tl -> if not (find_duplicate hd accu) then uniq_accu tl (hd::accu) else uniq_accu tl accu
  in reverse (uniq_accu lst []);;

(*uniq [5;6;5;4];;*)
(*uniq [4;4;5;6;7;7;6;5;4;3;2;1];;*)
