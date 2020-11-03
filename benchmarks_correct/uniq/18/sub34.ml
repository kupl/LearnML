let rec isin : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
    |[] -> false
    |h::t -> if h = a then true else isin a t;;

let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec u: 'a list -> 'a list -> 'a list
  = fun s l ->
    match l with
      |[] -> s
      |h::t -> if isin h s then u s t else u (s@[h]) t
    in
    u [] lst;;
