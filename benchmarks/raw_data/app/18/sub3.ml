let rec check : int -> 'a list -> 'a list = fun toCheck lst ->
  match lst with
    | [] -> [toCheck]
    | hd::tl -> if (toCheck = hd) then []
                else check toCheck tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  match l1 with
    | [] -> l2
    | hd::tl -> app tl (l2 @ (check hd l2));;
