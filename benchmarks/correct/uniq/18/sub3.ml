let rec check : int -> 'a list -> 'a list = fun toCheck lst ->
  match lst with
    | [] -> [toCheck]
    | hd::tl -> if (toCheck = hd) then []
                else check toCheck tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  let rec tmp : 'a list -> 'a list -> 'a list = fun src dest ->
    match src with
      | [] -> dest
      | hd::tl -> tmp tl (dest @ check hd dest) in tmp lst [];;
