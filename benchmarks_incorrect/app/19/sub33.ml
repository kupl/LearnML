let rec app1
= fun x l6 ->
  match l6 with
    | [] -> false
    | h2 :: t2 -> if x = h2 then true
                else app1 x t2;;
let rec app2 
= fun l3 l4 l5 ->
  match l3 with
    | [] -> l5
    | h1 :: t1 -> if app1 h1 l4 = true then app2 t1 l4 l5
                  else app2 t1 l4 (l5 @ [h1]);;
                  
let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  l2 @ (app2 l1 l2 []);; 

app [4;5;6;7] [1;2;3;4];;