

let uniq : 'a list -> 'a list
= fun lst ->  let rec tail_uniq a lst =
    match lst with
      | [] -> a
      | hd::tl -> tail_uniq (hd::a) (List.filter (fun x -> x  != hd) tl) in tail_uniq [] lst;;
  
  let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq (l2@l1);;

  

app [4;5;6;7] [1;2;3;4];; 
