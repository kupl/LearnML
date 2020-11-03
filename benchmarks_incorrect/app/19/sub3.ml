let head : 'a list -> 'a
= fun l -> match l with
  | [] -> raise (Failure "empty list")
  | hd::tl -> hd;;
  
let tail : 'a list -> 'a list
= fun l -> match l with
  | [] -> []
  | hd::tl -> tl;;
  
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

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> uniq l2
  | hd::tl -> if l2 = [] then uniq l1 else if hd < (head l2) then hd::(app (uniq tl) (uniq l2))
                else if hd > (head l2) then (head l2)::(app (uniq l1) (uniq (tail l2)))
                  else uniq (app tl l2);;
                  
app [4;5;5;5;6;7;] [1;2;3;4;8;];;