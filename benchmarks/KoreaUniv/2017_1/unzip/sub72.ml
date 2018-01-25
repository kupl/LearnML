(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list =
  fun lst -> match lst with
  [] -> ([], [])
  | hd :: tl ->
    (match tl with
    [] -> ([fst hd], [snd hd])
    | _ ->let merge m n = ([fst m] @ fst n, [snd m] @ snd n) in
      merge hd (unzip tl));;
