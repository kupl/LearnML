type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
= fun e ->
    let rec xcal : exp -> (int option -> int)
    = fun ee ->
        match ee with
        | X -> (fun q -> match q with
                        | Some a -> a
                        | None -> raise (Failure " no argument for X " ))
        | INT n -> (fun q -> n)
        | ADD (a , b) -> (fun q -> xcal_sub a b (+) q)
        | SUB (a , b) -> (fun q -> xcal_sub a b (-) q)
        | MUL (a , b) -> (fun q -> xcal_sub a b ( * ) q)
        | DIV (a , b) -> (fun q -> xcal_sub a b (/) q)
        | SIGMA (a , b , c) -> (fun q -> let a' = xcal a q in
                                     let b' = xcal b q in
                                     sigma_sub a' b' c)
    and sigma_sub : int -> int -> exp -> int
    = fun n m c ->
        let c' = xcal c in
        if n > m
            then raise (Failure "Inappropriate sigma-inputs")
            else if n = m
                    then c' (Some n)
                    else (c' (Some n)) + (sigma_sub (n + 1) m c)
    and xcal_sub : exp -> exp -> (int -> int -> int) -> (int option -> int)
    = fun a b bop -> let a' = xcal a in
                     let b' = xcal b in
                     (fun q -> bop (a' q) (b' q)) in
    xcal e None


        (*
    let rec xcal : exp -> (int -> int)
    = fun ee ->
        match ee with
        | X -> (fun q -> q)
        | INT n -> (fun q -> n)
        | ADD (a , b) -> (fun q -> xcal_sub a b (+) q)
        | SUB (a , b) -> (fun q -> xcal_sub a b (-) q)
        | MUL (a , b) -> (fun q -> xcal_sub a b ( * ) q)
        | DIV (a , b) -> (fun q -> xcal_sub a b (/) q)
        | SIGMA (a , b , c) -> (fun q -> let a' = xcal a q in
                                         let b' = xcal b q in
                                         sigma_sub a' b' c)
    and sigma_sub : int -> int -> exp -> int
    = fun n m c ->
        let c' = xcal c in
        if n > m
           then raise (Failure "Inappropriate sigma-inputs")
           else if n = m
                   then c' n
                   else (c' n) + (sigma_sub (n + 1) m c)
    and xcal_sub : exp -> exp -> (int -> int -> int) -> (int -> int)
    = fun a b bop ->
        let a' = xcal a in
        let b' = xcal b in
        (fun q -> bop (a' q) (b' q))
    in
    xcal e 0
    *)
