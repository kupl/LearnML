exception Problem

(*problem7*)

type digit = ZERO | ONE
type bin = digit list


let rec expo : int -> int -> int
= fun p q ->
if q>0 then p*expo p (q-1)
else if q=0 then 1
else raise Problem

let rec list_length : 'a list -> int
= fun l ->
match l with
|[] -> 0
|hd::tl -> 1 + list_length tl


let rec convert_deci : bin -> int
= fun b -> 
match b with
|[]-> 0
|hd::tl -> 
			if hd= ONE then (expo 2 (list_length tl) + convert_deci tl)
			else convert_deci tl

let rec convert_bin : int -> bin
=fun i ->
if (i /2) >= 1 && i mod 2 =1 then 	convert_bin (i/2) @[ONE]
else if(i/2)>=1 && i mod 2 =0 then convert_bin(i/2) @[ZERO]
else if i/2 =1/2 then [ONE]
else raise Problem

let bmul : bin -> bin -> bin
= fun b1 b2 ->
convert_bin (convert_deci b1 * convert_deci b2)
























