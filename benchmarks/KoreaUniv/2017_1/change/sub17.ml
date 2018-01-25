(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
  let rec impl l n =
    let rec loop m =
      match l with
      | [] ->
      if n = 0 then 1
      else 0
      | hd::tl ->
      if n < 0 || n < hd * m then 0
      else (impl tl (n - (hd * m))) + (loop (m + 1)) in
    loop 0 in
  impl coins amount;;
