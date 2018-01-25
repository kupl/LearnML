exception Problem

(* problem 7*)

let rec tuple_value
= fun f lst -> match lst with
             | [] -> []
             | hd::tl -> (f hd)::(tuple_value f tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
         | [] -> raise Problem
         | _ -> (tuple_value (fun (x,_)->x) lst, tuple_value (fun (_,x)->x) lst)