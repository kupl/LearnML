let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | h::t -> 
    let rec erase a l =
      match l with
        | [] -> []
        | h::t ->
          if a=h then t
          else h::erase a t
    in
    h::erase h (uniq t);;


uniq [1;1;1;12;2;2;2;1;12312;2];;