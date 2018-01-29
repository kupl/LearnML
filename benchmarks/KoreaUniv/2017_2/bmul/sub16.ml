(*Problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec expt : int -> int -> int
= fun a n -> if (n = 0) then 1
  else (if (n mod 2 = 0) then (expt a (n/2))*(expt a (n/2)) else (if (n=1) then a else a*(expt a (n-1))))


let rec bitode : bin -> int
= fun b-> let len = List.length(b) in
match b with
    |[]->0
    |hd::tl -> if(hd=ZERO) then bitode tl 
      else if(hd=ONE) then (expt 2 (len-1)) + bitode tl
      else 0
  

let rec detobi : int -> bin
= fun d -> if (d = 0) then [ZERO] else if (d = 1) then [ONE]
else if ( d mod 2 = 1 ) then (detobi (d/2))@[ONE]
else (detobi (d/2))@[ZERO]



let bmul : bin -> bin -> bin
= fun b1 b2 -> detobi (bitode b1 * bitode b2)

