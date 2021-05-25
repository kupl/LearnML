let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> 
    let rec uniq lst =
      match lst with
        | [] -> []
        | h::t -> 
          let rec erase a l =
            match l with
              | [] -> []
              | h::t ->
                if a=h then t
                else h::erase a t
          in
          h::erase h (uniq t)
    in
    uniq l2
  | h::t -> app t (l2@[h]);;

app [4;5;6;7] [1;2;3;4];;
app [2;2;10;123123;2;1;0;2;29;9;2] [2;4;2;2;2;3;2;9;2;];;
app [] [];;
