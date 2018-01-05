
(*
   컴퓨터공학부 2012-11270  장선웅
   hw 1 - Exercise 1
*)

let rec merge (l1, l2) = 
    if List.length l1 = 0   then l2
    else if List.length l2 = 0 then l1
    else if List.hd l1 > List.hd l2 then
        List.hd l1 :: merge(List.tl l1, l2)
    else List.hd l2 :: merge(l1, List.tl l2)


