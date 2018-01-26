(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

(* Helper functions for problem 7*)
let addChar s c = s ^ String.make 1 c

let divTwoRemainder dec = (dec - ((dec / 2)*2))

let rec bin_of_dec bin dec =
  if dec = 0 then bin
  else begin
    match (divTwoRemainder dec) with
      |0 -> bin_of_dec (ZERO::bin) (dec/2)
      |1 -> bin_of_dec (ONE::bin) (dec/2)
      |_ -> [ZERO]
    end

let char_of_digit digit =
  match digit with
    |ZERO -> '0'
    |ONE -> '1'

let rec string_of_digitlst str digitlst =
  match digitlst with
    |[] -> str
    |h::t -> string_of_digitlst (addChar str (char_of_digit h)) t
(* End of helper functions for problem 7 *)

let bmul : bin -> bin -> bin
= fun b1 b2 -> bin_of_dec [] ((int_of_string (string_of_digitlst "0b" b1))*(int_of_string (string_of_digitlst "0b" b2)))