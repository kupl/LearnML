type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec checklist ((arlist : 'a list), (stlist : 'a list)) : bool =
  let rec checkst ((arlst : 'a list), st) : bool =
    match (arlst, st) with
    | [], _ -> false
    | a :: b, c -> if a = c then true else checkst (b, c)
  in

  match (arlist, stlist) with
  | _, [] -> true
  | a, b :: c -> if checkst (a, b) then checklist (a, c) else false


let rec check (met : lambda) : bool =
  let rec makestlist (me : lambda) : string list =
    match me with
    | V a -> [ a ]
    | P (a, b) -> makestlist b
    | C (a, b) -> makestlist a @ makestlist b
  in

  let rec makearlist (me : lambda) : string list =
    match me with
    | P (a, b) -> a :: makearlist b
    | C (a, b) -> (
        match (a, b) with
        | V c, V d -> []
        | V c, P (d, e) -> d :: makearlist e
        | V c, C _ -> makearlist b
        | P (c, d), V e -> c :: makearlist d
        | P (c, d), P (e, f) -> (c :: e :: makearlist d) @ makearlist f
        | P (c, d), C _ -> (c :: makearlist d) @ makearlist b
        | C _, V c -> makearlist a
        | C _, P (c, d) -> makearlist a @ (c :: makearlist d)
        | C _, C _ -> makearlist a @ makearlist b )
    | _ -> []
  in

  match met with
  | P (a, b) -> checklist (a :: makearlist b, makestlist b)
  | C (a, b) -> check a && check b
  | _ -> false
