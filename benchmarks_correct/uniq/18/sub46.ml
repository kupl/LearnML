let rec uniq : 'a list -> 'a list
= fun lst ->  (* TODO *)
  inputelement lst []
and inputelement : 'a list -> 'a list -> 'a list
= fun lst sol ->
  match lst with
    |[] -> sol
    |h::t -> (
        if checkelement h sol then inputelement t sol
        else inputelement t (sol@[h])
      )
and checkelement : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
    |[] -> false
    |h::t -> (
        if h=a then true
        else checkelement a t
      );;
      
uniq [5;6;5;4];;