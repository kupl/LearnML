(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length b = match b with
                  |[]->0
                  |hd::tl->1+(length tl);;

let rec reverse b = match b with
                  |[]->[]
                  |hd::tl->(reverse tl)@[hd];;

let rec btod b = match b with
                |[ONE]->1
                |[ZERO]->0
                |hd::tl->(if hd=ONE then 1+2*(btod tl)
                        else 2*(btod tl));;
                

let rec dtob b = match b with
                  |0->[ZERO]
                  |1->[ONE]
                  |n ->(if (n mod 2 = 0) then (dtob (n/2))@[ZERO]
                        else (dtob (n/2))@[ONE]);;
                 
let bmul : bin -> bin -> bin
= fun b1 b2 -> let br1 = reverse b1 in
               let br2 = reverse b2 in
               dtob((btod br1)*(btod br2));;

