(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec reverse l = 
  match l with
  | [] -> []
  | hd::tl -> ((reverse tl)@[hd])


let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl


let rec append l cnt = if (cnt>0) then (append (ZERO::l) (cnt-1)) else l

let rec zero_check = fun b -> match b with
                              | [] -> false
                              | (hd::tl) -> if hd=ONE then true else (zero_check tl)

let bitsum : digit -> digit -> digit -> digit
= fun b1 b2 carry -> if (b1=ZERO&&b2=ZERO&&carry=ONE) then ONE else
                     if (b1=ZERO&&b2=ONE&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ZERO&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ONE) then ONE else ZERO


let bitcarry : digit -> digit -> digit -> digit
= fun b1 b2 carry -> if (b1=ZERO&&b2=ONE&&carry=ONE) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ZERO) then ONE else
                     if (b1=ONE&&b2=ZERO&&carry=ONE) then ONE else
                     if (b1=ONE&&b2=ONE&&carry=ONE) then ONE else ZERO


let rec bsum : bin -> bin -> digit -> bin
= fun b1 b2 carry -> (match b1 with
                      | [] -> (match carry with
                               | ZERO -> []
                               | ONE -> [ONE]
                               )
                      | (hd1::tl1) -> (match b2 with
                                       | [] -> b1
                                       | (hd2::tl2) -> (bitsum hd1 hd2 carry)::(bsum tl1 tl2 (bitcarry hd1 hd2 carry))
                                      )
                     )

let calc : bin -> bin -> bin
= fun b1 b2 -> let v1 = reverse (append b1 ((length b2)-(length b1))) in
               let v2 = reverse (append b2 ((length b1)-(length b2))) in
               (reverse (bsum v1 v2 ZERO))

let rec calc_mul = fun b1 b2 -> (match b2 with
                                | [] -> []
                                | (hd::tl) -> if hd=ZERO then (calc_mul (b1@[ZERO]) tl) else (calc b1 (calc_mul (b1@[ZERO]) tl))
                                )

let bmul : bin -> bin -> bin
= fun b1 b2 -> if ((zero_check b1)&&(zero_check b2)) then (calc_mul b1 (reverse b2)) else [ZERO]

