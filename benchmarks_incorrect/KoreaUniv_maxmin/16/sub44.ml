(*********************)
(*     Problem 1     *)
(*********************)

let rec max lst = let rec fold f l a = 
                        match l with
                        | [] -> a
                        | hd::tl -> f hd (fold f tl a)
in fold (fun x y ->if(x > y) then x else y) lst 0;;