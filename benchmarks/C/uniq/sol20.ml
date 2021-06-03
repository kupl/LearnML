let rec find :  'a -> 'a list -> bool 
= fun x lst ->
  match lst with
    | [] -> false
    | hd::tl -> (x = hd)||find x tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match List.rev lst with
    | [] -> []
    | hd::tl -> if find hd tl then uniq (List.rev tl) else (uniq (List.rev tl))@[hd];;
    
uniq [5;6;5;4];;