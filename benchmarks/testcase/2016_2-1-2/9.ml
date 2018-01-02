let rec f lst = let rec fold func l a =
                        match l with
                        | [] -> a
                        | hd::tl -> func hd (fold func tl a)
in fold (fun x y -> if(x < y) then x else y) lst 0;;