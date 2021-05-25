let rec del : 'a list -> 'a -> 'a list
= fun lst e -> match lst with
  | [] -> []
  | hd::tl -> if tl = [] && hd = e then []
                else if tl = [] then [hd]
                  else if hd = e then del tl e else hd::(del tl e);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (del tl hd));;
                
uniq [5;6;5;4;5;5;6;2;];;