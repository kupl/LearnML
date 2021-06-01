let rec comp : int -> 'a list -> 'a list
= fun n l ->
  match l with
    | [] -> []
    | h::t -> if n = h then (comp n t) else h::(comp n t);;

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    | [] -> []
    | h1::t1 -> [h1] @ uniq (comp h1 t1);;
      
uniq [5;6;5;4];;