let rec elm = fun lst a -> (*lst에 a 존재하면 리스트 안의 a만 골라서 없앰*)
match lst with 
  |[]-> []
  |hd::tl-> if hd = a then elm tl a else hd::(elm tl a);; 

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> []@
(match (l1, l2) with
  |([], l2)-> l2
  |(l1, [])-> l1
  |(h1::t1, h2::t2)-> h2::(app (elm (h1::t1) h2) t2))