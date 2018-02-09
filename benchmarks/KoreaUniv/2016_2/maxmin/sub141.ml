(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun list -> 
  match list with
   | [] -> raise (Failure "input list is empty")
   | _ -> List.fold_left (fun a b ->
          if a > b then a else b) (List.hd list) list

let rec min : int list -> int
= fun list ->
   match list with
   | [] -> raise (Failure "input list is empty")
   | _ -> List.fold_left (fun a b ->
          if a < b then a else b) (List.hd list) list
