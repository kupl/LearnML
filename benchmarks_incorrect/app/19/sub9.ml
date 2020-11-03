
let lis1 = [7;8;8;9];;
let lis2 = [1;8;9;10];;


let rec removeElement : 'a -> 'a list -> 'a list 
= fun ele l ->
  match l with 
   | [] -> []
   | h::t -> if h = ele then removeElement ele t
   else h :: (removeElement ele t)
;;


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with 
   | [] -> l2
   | h::t -> app t (h :: (removeElement h l2))
;;

app lis1 lis2;;