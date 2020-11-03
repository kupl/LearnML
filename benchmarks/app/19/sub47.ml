let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec elechk n lst =
  match lst with
  |[] -> false
  |h::t -> if h = n then true
           else elechk n t in
  let rec insert : 'a -> 'a list -> 'a list
  = fun n lst ->
  match lst with
  |[]->[n]
  |hh::tt -> if n < hh then n::hh::tt
           else hh::insert n tt in
  let rec sorting : 'a list -> 'a list
  =fun lst->
  match lst with
  |[]->[]
  |h::t -> insert h (sorting t) in
  match lst with
  |[] -> []
  |h::t -> if elechk h t then uniq t
           else sorting (h::uniq t) in
  match l1 with
  |[] -> l2
  |h::t -> uniq(h:: app t l2);;
  
  app [4;5;6;7] [1;2;3;4];;
  app [2;3;6;4] [6;3;5;9];;