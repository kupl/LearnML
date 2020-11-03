let rec filter f = function
  | [] -> []
  | h::t -> if f h then h::(filter f t) else filter f t

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    |[] -> []
    |h::t -> if filter (fun x -> x != h) t = t then h::(uniq t)
                            else h :: filter (fun x -> x != h) t;;

uniq [5;6;5;4];;