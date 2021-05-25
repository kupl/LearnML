let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    | [] -> []
    | h::t -> 
      let rec filter hd l =
        match l with
          | [] -> []
          | h::t -> 
            if hd = h then filter hd t
            else h::(filter hd t)
      in h::(uniq (filter h t));;
uniq [5;6;5;4];;
uniq [1;2;3;4;5;6;1;3;5];;
