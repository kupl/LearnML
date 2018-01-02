(*remove duplicate https://stackoverflow.com/questions/10271711/ocaml-append-list-to-another-list-without-duplicated*)

let rec find e l =
  match l with
    |[] -> false
    |(h::t) -> if (h = e) then true else find e t
;;

(* helper function append l1 to l2 without duplicate *)
let rec help_append_list l1 l2 =
  match l1 with
    |[] -> l2
    |(h::t) -> if (find h l2 = false) then (help_append_list t (h::l2)) else (help_append_list t l2)
;;

let f l1 l2 = help_append_list l1 l2;;