let rec uniq1
= fun x l1 ->
  match l1 with
    | [] -> false
    | h1 :: t1 -> if x = h1 then true
                else uniq1 x t1;;

let rec uniq2
= fun l2 l3 ->
  match l2 with
    | [] -> l3
    | h :: t -> if uniq1 h l3 = true then uniq2 t l3
                else uniq2 t (l3 @ [h]);;
  
let uniq : 'a list -> 'a list
= fun lst -> 
  uniq2 lst [];;

uniq [5;6;5;4];;
