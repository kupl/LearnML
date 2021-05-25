let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec iscontain l k =
    match l with
      | [] -> false
      | hd::tl -> if hd = k then true else iscontain tl k in
    match l1 with
      | [] -> l2
      | hd::tl -> if (iscontain l2 hd) then (app tl l2) 
      else (app tl (l2@[hd]));;
      
app [4;5;6;7] [1;2;3;4];;