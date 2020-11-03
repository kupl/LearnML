let rec dup a lst =
  match lst with
    [] -> false
    | h::t -> if a = h then true else dup a t;;

let rec insert a lst ans =
  if dup a ans then match lst with
                    [] -> ans
                    | h::t -> insert h t ans
                else match lst with
                      [] -> ans@[a]
                      | h::t -> insert h t (ans@[a]);;

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
    match lst with
      [] -> []
      | h::t -> insert h t [];;
 
(*
이해가 쉬운 버전, 초기에는 이렇게 작성함
let rec dup a lst =
  match lst with
    [] -> false
    | h::t -> if a = h then true else dup a t;;

let insert a lst =
  if dup a lst then lst else a::lst;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
    match lst with
      [] -> []
      | h::t -> insert h (uniq t)
*)

uniq [5;6;5;4];;