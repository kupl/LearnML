(*problem 1*)
module Problem1 = struct
type aexp = 
	| Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list


let rec diff : aexp*string -> aexp
	= fun (ae,va) -> match ae with
	| Const a -> Const 0
	| Var x -> if x = va then Const 1 else Const 0
	| Power (x,a) -> if x = va then Times[Const a; Power(x, a-1)] else Const 0
	| Times [] -> Const 0
	| Times (hd::[]) -> diff(hd,va)
	| Times (hd::tl) -> Sum[Times(diff(hd,va)::tl); Times[hd; diff(Times tl,va)]]
	| Sum [] -> Const 0
	|	Sum (hd::[]) -> diff(hd,va)
	| Sum (hd::tl) -> Sum[diff(hd,va); diff(Sum tl, va)] 

end
(*problem 2*)
module Problem2 = struct
type mobile = branch * branch (*left and rigth branches*)
and branch = SimpleBranch of length * weight
					 | CompoundBranch of length * mobile
and length = int
and weight = int


let rec sumwe
	= fun a -> match a with
	| SimpleBranch (leng,weig) -> weig
	| CompoundBranch (leng,(left,right)) -> (sumwe left) + (sumwe right)


let mul
	= fun a -> match a with
	| SimpleBranch (leng,weig) -> leng*weig
	| CompoundBranch (leng, (left,right)) -> leng*((sumwe left) + (sumwe right))

let rec balanced : mobile -> bool
	= fun (left,right) -> match left with
	| SimpleBranch (leng1,weig1) -> (match right with
																	| SimpleBranch(leng2,weig2) -> if ((mul left) = (mul right)) then true else false
																	| CompoundBranch(leng2,(left1,right1)) -> if (( balanced(left1,right1) = true) && ((mul left) = (mul right))) then true else false)
	| CompoundBranch (leng1,(left1,right1)) -> if (balanced(left1,right1) = false)  then false else ( match right with
																																															| SimpleBranch(leng2,weig2) -> if ((mul left) = (mul right)) then true else false
																																															| CompoundBranch(leng2,(left2,right2)) -> if (balanced(left2,right2) = false) then false else if ((mul left) = (mul right)) then true else false)

end
(*problem 3*)
module Problem3 = struct
type exp = X
				 | INT of int
				 | ADD of exp * exp
				 | SUB of exp * exp
				 | MUL of exp * exp
				 | DIV of exp * exp
				 | SIGMA of exp * exp * exp


let rec calculator : exp -> int
	= fun exp -> match exp with
	| X -> raise(Failure "Wrong input")
	| INT a -> a
	| ADD (a,b) -> calculator a + calculator b
	| SUB (a,b) -> calculator a - calculator b
	| MUL (a,b) -> calculator a * calculator b
	| DIV (a,b) -> calculator a / calculator b
	| SIGMA (first, last, poly) ->
		let n = calculator first in
			let m = calculator last in
			if n = m then
			begin
			let rec help 
				= fun p l ->	match p with
				| X -> help l l
				| INT a -> a
				| ADD (a,b) -> (help a l) + (help b l)
				| SUB (a,b) -> (help a l) - (help b l)
				| MUL (a,b) -> (help a l) * (help b l)
				| DIV (a,b) -> (help a l) / (help b l)
				| SIGMA (f, las, po) -> calculator p
				in help poly last
				end
			else
			calculator (SIGMA (first, SUB(last, INT 1), poly)) + calculator(SIGMA (last, last, poly))
			end

(*problerm 4*)
module Problem4 = struct
type exp = V of var
				 | P of var * exp
				 | C of exp * exp
and var = string


let rec fv 
	= fun (va,la) -> match la with
	|[] -> false
	| hd::tl -> if (va = hd) then true else fv(va,tl)

let rec findv
	= fun(a,la) -> match a with
	|V((b:var)) -> fv(b,la)
	|P((va:var),a1)-> findv(a1,(la @ [va]))
	|C(a1,a2) -> if (findv(a1,la)&&findv(a2,la)) = true then true else false;;

let check : exp -> bool
= fun a -> findv(a,[]);;

end


 
