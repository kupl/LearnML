let prime = fun n -> let a = n in
        let rec isDivisable d = if d < 2 then false else
                                if (a mod d) = 0 then true || isDivisable (d-1) else
                                                        false || isDivisable (d-1) in
                if a<2 then false else not (isDivisable (a-1));;
