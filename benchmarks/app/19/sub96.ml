(**)
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l2 with
    | [] -> l1
    | h::t -> 
      let rec check hd l =
        match l with
          | [] -> true
          | h::t ->
            if hd = h then false
            else check hd t
      in if check h l1 then h::(app l1 t)
        else app l1 t;;

app [4;5;6;7] [1;2;3;4];;
