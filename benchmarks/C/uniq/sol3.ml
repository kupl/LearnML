let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec remove_same n lst =
    match lst with
      | [] -> []
      | hd::tl -> if hd=n then remove_same n tl else hd::(remove_same n tl) in
  
  match lst with
    | [] -> []
    | hd::tl -> hd::(uniq (remove_same hd tl));;
    
uniq [1;1;2;2;1;3;4;4;3;5];;
