let rec elm = fun lst a -> (*lst에 a 존재하면 리스트 안의 a만 골라서 없앰*)
match lst with 
  |[]-> []
  |hd::tl-> if hd = a then elm tl a else hd::(elm tl a);; 

let rec uniq : 'a list -> 'a list
= fun lst -> []@(match lst with 
  |[]-> []
  |hd::tl-> hd::(elm (tl) hd)
  );;

uniq [5;6;5;4];;

(*
Write a function
uniq: ’a list -> ’a list
which removes duplicated elements from a given list so that the list contains
unique elements. For instance,
uniq [5;6;5;4] = [5;6;4]
*)