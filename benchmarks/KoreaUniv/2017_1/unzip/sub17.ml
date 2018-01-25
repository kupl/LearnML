(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)
  let rec map f l =
    match l with
    | [] -> []
    | hd::tl -> (f hd)::(map f tl) in
  ((map (fun p -> (match p with (x, _) -> x)) lst), (map (fun p -> (match p with (_, x) -> x)) lst));;