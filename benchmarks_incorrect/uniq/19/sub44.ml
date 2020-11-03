let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  let rec loop l n = 
    match l with
    | [] -> l
    | hd::tl -> 
      if (n = hd) then (loop tl n)
      else hd::(loop tl n) in
  match lst with
  | [] -> lst
  | hd::tl -> hd::(loop tl hd);;

uniq [5;6;5;4];;
(*let uniq : 'a list -> 'a list*)
(*= fun lst -> (* TODO *);;*)