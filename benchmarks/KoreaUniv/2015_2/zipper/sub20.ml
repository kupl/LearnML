let rec zipper (l1,l2) = 
  match l1 with
  |[]->l2
  |h1::t1 ->
    match l2 with
      |[]->l1
      |[h2]->if h1+0<h2+0 then zipper ([],([h1;h2])) else [h2]@(zipper (l1,[]))
      |h2::t2 -> if h1+0<h2+0 then zipper (t1,([h1]@([h2]@t2))) else [h2]@(zipper (l1,t2))
