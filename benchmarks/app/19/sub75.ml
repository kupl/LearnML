
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1, l2 with
   | [], _ -> l2
   | _, [] -> l1
   | h1 :: tl1, h2 :: tl2 ->
       if h1 < h2 then h1 :: app tl1 l2 else h2 :: app l1 tl2;;
    
app[4;5;6] [1;2;3];;