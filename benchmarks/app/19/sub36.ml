let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *);;
let rec insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> ( * if a< hd 
               then a::hd::tl
                else hd::(insert a tl);;* )