let rec remove : 'a -> 'a list -> 'alist
= fun a l ->
  match l with
  | [] -> []
  | hd::tl ->
    if a = hd then remove a tl
    else hd::(remove a tl)

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (remove hd tl));;
      
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  uniq (l2@l1);;