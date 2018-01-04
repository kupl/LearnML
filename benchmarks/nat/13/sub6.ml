(* KIHWAN KANG HW01-3 *)

(* PREDEFINED TYPES *)
type nat = ZERO | SUCC of nat
(* END OF PREDEFINED TYPES *)

let rec natadd (front, rear) = 
	match front with
	|ZERO -> rear
	|SUCC front_rest -> SUCC (natadd (front_rest, rear))

let rec natmul (front, rear) = 
	match front with
	|ZERO -> ZERO
	|SUCC front_rest -> natadd (rear, natmul (front_rest, rear))
