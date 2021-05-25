let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec uniqapp lst1 lst2 = 
    let rec dupl x l= 
      match l with
        |[] -> false
        |hd::tl -> (hd=x)||(dupl x tl)
      in
    match lst1 with
      |[] -> lst2
      |hd::tl -> if dupl hd lst2 then uniqapp tl lst2 else uniqapp tl (lst2@[hd])
    in
    uniqapp l1 (uniqapp l2 [])
;;

app [4;5;6;7] [1;2;3;4];;
app [1;1;1;1] [2;2;2;2];;