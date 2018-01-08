
(* ex 1 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
		  | Poland | Portugal | Italy | Germany | Sweden | England
		  | Croatia | Argentina | A | B

type tourna = LEAF of team
			| NODE of tourna * tourna

(*
let example = NODE(NODE(LEAF Korea, LEAF France),NODE(NODE(LEAF Germany,LEAF Sweden),LEAF England))
let ex2 = NODE(NODE(LEAF Korea,NODE(LEAF France,LEAF Argentina)),NODE(NODE(LEAF Germany,LEAF Sweden),LEAF England))
*)

(*
let rec draw tna = 
	match tna with 
	| LEAF l -> print_string "|"
	| NODE(l,r) -> begin 
				   print_string "|";
				   print_newline()
				   (draw l) ^ (draw r)
				   end
*)

let power n = 
	(int_of_float ((2.0)**(float_of_int n)))

let rec depth tna = 
	match tna with
	| LEAF _ -> 1 
	| NODE(l,r) -> if (depth l) >= (depth r) then ((depth l) + 1) else ((depth r) + 1)

let rec l_or_n(a,i) = 
	match (List.nth a i) with
	| (_,0,_,_) -> a
	| (LEAF l,d,d2,d3) -> l_or_n(a@[(LEAF l,d-1,d2,d3+(power (d-3)) )],i+1)
	| (NODE(l,r),d,_,_) -> l_or_n(a@[(l,d-1,d-1,0)]@[(r,d-1,d-1,0)],i+1)

let rec draw_elt(a,b,e) =
	if a<b then 
		begin 
		print_string e;
		draw_elt(a+1,b,e)
		end

let rec draw_node i = 
	begin
	print_string "|";
	draw_elt(0,i,"-");
	print_string "|"
	end

let rec draw_leaf j = 
	begin
	draw_elt(0,j," ");
	print_string "|";
	draw_elt(0,j," ")
	end

let rec draw(data,i,p) = 
	let(tree,d,d2,d3) = (List.nth data i) in 
	
	if d>1 then
		begin
		(if p!=d then
			begin
			print_newline(); 
			draw_elt(0,(power (d-2)), " ")
			end
		);
		(match tree with 
		| LEAF _-> (draw_leaf ((power (d2-2)) + d3))
		| NODE(_,_) -> (draw_node ((power (d-1))-1))
		);
		draw_elt(0,((power (d-1))-1)," ");
		draw(data,i+1,d)
		end
let rec pptree tna =
	let d = (depth tna) in
	let temp = [(tna,d,d,0)] in 
	
	begin
	draw_elt(0,(power (d-1))," ");
	print_string "|";
	draw((l_or_n(temp,0)),0,-1) 
	end

(*
4 8 15
3 4 7 4
2 2 3 6
1 1 1 8

(power (d-1))
*)


