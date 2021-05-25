
let rec dupl a l =
  match l with
    |[] -> false
    |hd::tl -> if hd = a then true else dupl a tl;;
    

let rec app l1 l2 =
  match l1 with
    |[] -> l2
    |hd::tl ->
      if dupl hd l2 then app tl l2
      else app tl (l2 @ [hd]);;
                
app [4;5;6;7] [1;2;3;4];;

