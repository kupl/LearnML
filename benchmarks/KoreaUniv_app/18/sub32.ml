

let rec check = fun x lst ->
  match lst with
    |[] ->false
    |hd::tl ->(hd=x) || (check x tl);; 


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
    match l2 with
    |[]->l1
    |hd::tl -> if check hd l1 then app l1 tl
              else app (l1@[hd]) tl;;
  


app [1;2;3] [3;4;5];;
