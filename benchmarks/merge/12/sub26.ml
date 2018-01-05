let rec merge (l1,l2) = 
  match (l1,l2) with
  | ([], []) -> []
  | ([], _ ) -> l2
  | (_, [] ) -> l1
  | (hd1::tl1, hd2::tl2) ->
      if hd1 > hd2 then hd1::(merge (tl1,l2))
      else hd2::(merge(l1, tl2))
(*
let a = [100;73;66;54;23;11;8;5;1]
let b = [88;76;71;49;44;23;11;3]

let _ = print_string "["; 
        List.iter (Printf.printf "%d;") (merge (a,b));
        print_string "]\n"
        *)
