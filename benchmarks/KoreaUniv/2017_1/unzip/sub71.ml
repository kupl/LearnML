(* problem 7*)
let rec unzip_helper : ('a * 'b) list -> 'a list -> 'b list -> 'a list * 'b list
= fun l al bl ->
  match l with
  | [] -> (al, bl)
  | hd::tl -> let a = fst hd in
              let b = snd hd in
              unzip_helper tl (al @ [a]) (bl @ [b])

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> unzip_helper lst [] []