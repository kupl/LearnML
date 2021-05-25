let rec remove_same a l =
  match l with 
    | [] -> []
    | hd::tl -> if a = hd then remove_same a tl 
                else [hd] @ remove_same a tl;;


let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    | [] -> []
    | hd::tl -> [hd] @ uniq (remove_same hd lst);; 

uniq [5;6;8;4;7;6;4;4;4;2;3;4;5;];;