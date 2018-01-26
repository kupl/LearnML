(* problem 7*)
type digit = ZERO | ONE;;
type bin = digit list;;

let digitize : int -> digit
= fun n -> match n with
            |0 -> ZERO
            |_ -> ONE;;

let rec getbin : int -> bin
= fun n ->
  let rec dtob : int -> bin -> bin
  = fun a l -> match a with
                |0 -> l
                |_ -> dtob (a / 2) ((digitize (a mod 2)) :: l)
  in dtob n [];;

let rec exp : int -> int
= fun n -> match n with
            |0 -> 1
            |_ -> 2 * exp (n - 1);;


let rec count : bin -> int
= fun l -> match l with
            |[] -> 0
            |hd :: tl -> 1 + (count tl);;

let rec btod : bin -> int
= fun l -> match l with
            |[] -> 0
            |hd :: tl  -> if hd = ZERO then (btod tl) else (exp ((count l) - 1)) + btod tl;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> getbin ((btod b1) * (btod b2));;
                
                

