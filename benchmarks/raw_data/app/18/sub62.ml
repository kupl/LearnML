let rec filter f l =
  let rec aux s f l = match l with
    [] -> s
    | hd::tl -> if (f hd) then (s @ (hd::(filter f tl))) else (s @ (filter f tl))
    in aux [] f l;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  [] -> []
  | hd::tl -> hd :: (uniq (filter (fun x -> x<>hd) tl));;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq l2@l1;;