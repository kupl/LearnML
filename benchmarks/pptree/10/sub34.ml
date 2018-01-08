(* 4190.310 Programming Language 			*
 * Homework #2 - Exercise 1 (이쁘개)		 *
 * 2008-11744 Jongwook Choi 				*)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

(* debug : from HW1 *)

let rec parenize x = 
	let toString t = match t with
		  Korea -> "Korea"
		| France -> "France"
		| Usa -> "Usa"
		| Brazil -> "Brazil"
		| Japan -> "Japan"
		| Nigeria -> "Nigeria"
		| Cameroon -> "Cameroon"
		| Poland -> "Poland"
		| Portugal -> "Portugal"
		| Italy -> "Italy"
		| Germany -> "Germany"
		| Sweden -> "Sweden"
		| England -> "England"
		| Croatia -> "Croatia"
		| Argentina -> "Argentina"
	in
	match x with
	  LEAF t -> toString(t)
	| NODE (l, r) -> "(" ^ parenize(l) ^ " " ^ parenize(r) ^ ")"


(* HW2 begins here *)
(* sub-quadratic algorithm is hard to write in damn ML *)
open List ;;
open Queue ;;
open Map ;;

module OrderedInt = struct type t = int let compare = compare end	;;
module IntMap = Map.Make ( OrderedInt )	;;
exception InvalidOperationError ;;

let rec pptree (t : tourna) =
	let n = ref 0 in
	let map = ref(IntMap.empty) in 
	let adj = ref(IntMap.empty) in

	let rec getWinfo : tourna * int -> int * int * int * int = fun (t, leftmost) ->
		let tno = (n := !n + 1; !n) in
		match t with
			  LEAF _ -> (tno, leftmost, leftmost, leftmost)
			| NODE(x, y) ->
				let (xno, xleft, xmid, xright) = getWinfo(x, leftmost) in
				let (yno, yleft, ymid, yright) = getWinfo(y, xright + 1) in

				let shiftamt = ((xmid + ymid) mod 2) in
				let (yno, yleft, ymid, yright) = (yno, yleft + shiftamt, ymid + shiftamt, yright + shiftamt) in

				let _ = map := IntMap.add xno (xno, x, xleft, xmid, xright, 0) (!map) in
				let _ = map := IntMap.add yno (yno, y, yleft, ymid, yright, shiftamt) (!map) in

(*				let _ = (let old = try IntMap.find tno (!adj) with Not_found -> [] in 
					IntMap.add tno ((xno, x)::(yno, y)::old) (!adj)) in*)
				let _ = adj := IntMap.add tno [xno ; yno] (!adj) in
			
				(tno, xleft, (xmid + ymid) / 2, yright)
	in
	let rec adjust u offset = 
		let adju = try IntMap.find u (!adj) with Not_found -> [] in
		let (uno, utree, uleft, umid, uright, ushift) = IntMap.find u (!map) in
(*		let _ = Printf.printf "%d %d\n" u offset in	*)
		let _ = (map := IntMap.add u (u, utree, uleft + offset, umid + offset, uright + offset, ushift) (!map)) in
		match adju with
			  [] -> ()
			| [xno; yno] ->
				let _ = adjust xno (offset + ushift) in
				let _ = adjust yno (offset + ushift) in
				()
			| _ -> raise InvalidOperationError
	in	
	let bfs (startno : int) = 
		let queue = Queue.create() in
		let anslst = ref [] in
		let retrieve_mid u =
			let (_, _, _, r, _, _) = IntMap.find u (!map) in
			r
		in
		let rec bfsloop() = 
			if Queue.is_empty queue then ()
			else
				let (u, ulevel) = Queue.pop queue in
				let umid = retrieve_mid u in
				let adju = try IntMap.find u (!adj) with Not_found -> [] in
				let _ = match adju with
					  [] -> 
					  	anslst := (ulevel, umid, umid, umid) :: !anslst 			
					| [xno; yno] -> 
						let _ = (Queue.push (xno, ulevel+1) queue) in
						let _ = (Queue.push (yno, ulevel+1) queue) in
						let (xmid, ymid) = (retrieve_mid xno, retrieve_mid yno) in
						anslst := (ulevel, xmid, umid, ymid) :: !anslst
					| _ -> raise InvalidOperationError
				in bfsloop()
		in
			let _ = Queue.push (startno, 0) queue in
			let _ = bfsloop() in
			List.rev (!anslst)
	in
	let getMaxColRow lst =
		let maxrow = ref 0 in
		let maxcol = ref 0 in
		let rec aux l = match l with
			  [] -> () 
			| (h,l,m,r)::l' -> (
				maxrow := max (!maxrow) h;
				maxcol := max (!maxcol) r;
				aux l'
			) in
		let _ = aux lst in
		(!maxrow, !maxcol)
	in
	let printer lst = 
		let (maxrow, maxcol) = getMaxColRow lst in
		let isleaf = Array.create (maxcol+1) false in
		let rec printer_loop lst row col = 
			if row = maxrow && col = 0 then ()
			else match lst with
				  [] -> () 
				| (level, left, mid, right)::remain ->
					let _ = (if left=right then isleaf.(left) <- true) in
					if level > row then
					(
						for j = col to maxcol do
							if isleaf.(j) then print_char '|'
							else print_char ' '
						done;
						print_char '\n';
						printer_loop lst (row+1) 0
					)
					else
					(
						for j = col to right do
							if j = left then print_char '|'
							else if left < j && j < right then print_char '-'
							else if j = right then print_char '|'
							else if (isleaf.(j)) then print_char '|'	
							else print_char ' '
						done;
						printer_loop remain row (right+1)
					)
		in
		match lst with
			  [] -> ()
			| (_, _, mid, _)::_ -> (
				for j = 0 to (mid-1) do print_char ' ' done;
				print_string "|\n";
				printer_loop lst 0 0;
				)
	in	
		let (rootno, rootleft, rootmid, rootright) = getWinfo (t, 0) in
		let _ = map := IntMap.add rootno (rootno, t, rootleft, rootmid, rootright, 0) (!map) in
		
		let _ = adjust 1 0 in
		let lst = bfs(1) in
		printer lst	
;;	

