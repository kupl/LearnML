(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec reverse : bin -> bin
= fun b1 -> match b1 with
            | [] -> []
            | hd::tl -> (reverse tl)@[hd]

let rec bin_dec : bin -> int -> int
= fun b1 n -> match b1 with
              | [] -> 0
              | hd::tl -> if hd = ONE then (n + (bin_dec tl (n*2)))
                          else (bin_dec tl n*2)

let rec dec_bin : int -> bin -> bin
= fun n b2-> if n / 2 = 0 then (ONE::b2)
             else if n mod 2 = 0 then (dec_bin (n/2) (ZERO::b2))
             else dec_bin (n/2) (ONE::b2)

let bmul : bin -> bin -> bin
= fun b1 b2 -> let empty_bin = [] 
                in dec_bin ((bin_dec (reverse b1) 1) * (bin_dec (reverse b2) 1)) empty_bin
