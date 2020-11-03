let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  
  match l1 with
    [] -> []
    | h1::t1 ->
      match l2 with
        [] -> []
        | h2::t2 ->
          if h1 = h2 then t2
          else h2::(app t1 t2);;

app [4;5;6;7] [1;2;3;4];;
