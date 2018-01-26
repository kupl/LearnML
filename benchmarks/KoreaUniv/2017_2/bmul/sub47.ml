(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec b_reverse : bin -> bin
= fun b ->
        match b with
        | [] -> []
        | hd::tl -> (b_reverse tl)@[hd]

let badd b1 b2 =
        let b1_r = b_reverse b1 in
        let b2_r = b_reverse b2 in
        let rec badd_r br1 br2 c =
                match c with
                | ZERO ->
                       (match br1 with
                        | [] -> br2
                        | hd1::tl1 ->
                                match br2 with
                                | [] -> br1
                                | hd2::tl2 ->
                                        match (hd1, hd2) with
                                        | (ONE, ONE) -> ZERO::badd_r tl1 tl2 ONE
                                        | (ONE, ZERO) -> ONE::badd_r tl1 tl2 ZERO
                                        | (ZERO, ONE) -> ONE::badd_r tl1 tl2 ZERO
                                        | (ZERO, ZERO) -> ZERO::badd_r tl1 tl2 ZERO)
                | ONE -> badd_r (badd_r br1 [ONE] ZERO) br2 ZERO
        in
        let result_r = badd_r b1_r b2_r ZERO in
        b_reverse result_r

let rec remove_front_zero b =
        match b with
        | [] -> [ZERO]
        | ZERO::tl -> remove_front_zero tl
        | ONE::tl -> b

let rec bmul_rec b1 b2 r =
        match b2 with
        | [] -> r
        | ZERO::tl -> bmul_rec b1 tl (r@[ZERO])
        | ONE::tl -> bmul_rec b1 tl (badd (r@[ZERO]) b1)
let bmul : bin -> bin -> bin
= fun b1 b2 -> 
        let result = bmul_rec b1 b2 [ZERO]
        in remove_front_zero result
