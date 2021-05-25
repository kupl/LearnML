let rec find e = function
  | [] -> false
  | h::t -> h = e || find e t;;

let add : int -> 'a list -> 'a list
= fun e lst -> [e]@lst;;
 

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> lst
  | h::t -> if find h t then uniq t
            else add h (uniq t);;
            
uniq [5;6;5;4;3];;