let rec merge (a, b) =
  match a with
  | [] -> b
  | aHead :: aTail ->
    (match b with
     | [] -> a
     | bHead :: bTail ->
       if aHead >= bHead then
         aHead :: merge (aTail, b)
       else
         bHead :: merge (a, bTail))

(*
let testa = [15; 13; 11; 8; 7; 5; 2]
let testb = [25; 23; 14; 12; 10; 9; 6; 5; 4; 3; 2; 1]

let rec print_list l =
  match l with
  | [] -> print_endline ""
  | head :: tail -> print_int head; print_string " "; print_list tail 

let _ = print_list (merge (testa, testb))
*)
