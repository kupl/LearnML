let rec merge (list1,list2) = 
 match list1,list2 with
 | [],_ ->list2
 | _,[] ->list1
 |hlist1::tlist1,hlist2::tlist2->
  if hlist1>hlist2 then hlist1::merge (tlist1,list2) else hlist2 :: merge (list1,tlist2) 

