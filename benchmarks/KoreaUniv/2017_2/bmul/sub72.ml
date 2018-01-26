(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
  let rec lis : bin -> 'a list 
  = fun a ->
    match a with
    | []->[]
    | hd::tl -> 
      if (hd = ONE) then (lis tl)@[1]
      else (lis tl)@[0] in 
      let rec fastexpt : int -> int -> int
      = fun b n -> 
        if n = 0 then 1
        else if (n mod 2 = 0) then (fastexpt b (n/2)) * (fastexpt b (n/2))
        else b * (fastexpt b (n-1)) in 
        let rec dconvert : int -> 'a list -> int
        = fun c d ->
          match d with
          |[]->0
          | hd::tl -> (hd * (fastexpt 2 c))+(dconvert (c+1) tl) in 
          let rec bconvert : int -> bin -> bin
          = fun e f ->
            if e<1 then f
            else if (e mod 2) = 0 then bconvert (e/2) ([ZERO]@ f)
            else bconvert (e/2) ([ONE] @ f) in 
            let g = dconvert 0 (lis b1) in
            let h = dconvert 0 (lis b2) in
            bconvert (g*h) []









