(*onetime element https://stackoverflow.com/questions/9879287/find-unique-elements-in-a-list-in-ocaml*)
let rec uniq_help : int list -> int -> int list
= fun l n ->
  match l with
  |[] -> []
  | h::t -> if n = h then uniq_help t n else h::(uniq_help t n);;


let rec f : int list -> int list
= fun x ->
  match x with
  | [] -> []
  | hd::tl -> uniq_help tl hd 
;;
(*
let rec f x =
  let rec uniq_help l n = 
    match l with
    | [] -> []
    | h :: t -> if n = h then uniq_help t n else h::(uniq_help t n) 
  in
  match x with
  | [] -> []
  | h::t ->  h::(uniq_help (f t) h)
*)
