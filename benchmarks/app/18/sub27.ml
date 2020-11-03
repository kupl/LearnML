let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec comp : int -> 'a list -> 'a list = fun n l ->
   match l with
      | [] -> []
      | h::t -> if n = h then (comp n t) else h::(comp n t)
  in
  match l2 with
  | [] -> l1
  | h1::t1 -> h1::(app (comp h1 l1) t1);;
  

app [4;5;6;7] [1;2;3;4];;