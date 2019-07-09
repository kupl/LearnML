let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    |[]->l2
    |h1::t1->(
      match l2 with
        |[]->l1
        |h2::t2->(
          if h1<h2 then h1::(app t1 l2)
          else if h1>h2 then h2::(app l1 t2)
          else h1::(app t1 t2)
        )
      );;
      
app [4;5;6;7] [1;2;3;4];;