let rec isone : int -> 'a list -> bool
= fun l1 l2 ->
  match l2 with
    |[] -> false
    |h2::t2 -> 
      if l1 = h2 then true 
      else isone l1 t2;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l2 with
  | [] -> l1
  |h2::t2 -> 
    match l1 with
    | [] -> l2
    |h1::t1 ->
      if h1 > h2 then if (isone h2 t1) = false then h2::app l1 t2 else app l1 t2
      else if h1 < h2 then if (isone h1 t2) = false then h1::app t1 l2 else app t1 l2
      else app l1 t2;;
        
app [4;5;6;7] [1;2;3;4];;