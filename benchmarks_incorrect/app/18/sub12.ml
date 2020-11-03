let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec check_same n lst = 
    match lst with
      | [] -> false
      | hd::tl -> if hd=n then true else check_same n tl in
  
  let rec remove_same l1 l2 =
    match l1 with
      | [] -> []
      | hd1::tl1 -> if check_same hd1 l2 then remove_same tl1 l2 else hd1::(remove_same tl1 l2) in l2@(remove_same l1 l2);;
        
app [1;2;3;4] [1;7;3;4;5;6];;
