(* Name: Yoon Jaexp Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 2: Mathemadiga *)

(* 1. Provided declarations / definitions *)
type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list
(**)

(* 2. My code *)
let zero = Const(0)
let one = Const(1)

(* Helper function that simplifies an aexp *)
let rec simplifyAe : aexp -> aexp = fun exp -> (
	match exp with
	| Power(var,e) -> (
		match e with
		| 0 -> one
		| 1 -> Var(var)
		| _ -> exp
	)
	| Times(aexp_list) -> (
		match aexp_list with
		| [] -> one
		| h::[] -> simplifyAe(h)
		| h::t -> (
			match h with
			| Const(0) -> zero
			| Const(1) -> simplifyAe(Times(t))
			| _ -> (
				let t_simp = simplifyAe(Times(t)) in
				let h_simp = simplifyAe(h) in (
					match (h_simp, t_simp) with
					| (Const(0), _) -> zero
					| (_, Const(0)) -> zero
					| (Const(1), _) -> t_simp
					| (_, Const(1)) -> h_simp
					| (_, _) -> Times([h_simp;t_simp])
				)
			)
		)
	)
	| Sum(aexp_list) -> (
		match aexp_list with
		| [] -> zero
		| h::[] -> simplifyAe(h)
		| h::t -> (
			match h with
			| Const(0) -> simplifyAe(Sum(t))
			| _ -> (
				let t_simp = simplifyAe(Sum(t)) in
				let h_simp = simplifyAe(h) in (
					match (h_simp, t_simp) with
					| (Const(0), _) -> t_simp
					| (_, Const(0)) -> h_simp
					| (_, _) -> Sum([h_simp;t_simp])
				)
			)
		)
	)
	| _ -> exp
)

let rec removeIndex: aexp list * int -> aexp list = (
	fun (l, index_to_remove) ->
		if index_to_remove = 0 then List.tl l
		else (List.hd l)::(removeIndex(List.tl(l),index_to_remove - 1))
)

let getPartialProduct : aexp list * aexp list * int -> aexp = (
	fun (l_orig, l_diff, index) ->
		let elem_diff : aexp = List.nth l_diff index in
		let l_orig_without_index : aexp list = removeIndex(l_orig,index) in
		let product : aexp list = elem_diff::l_orig_without_index in
			Times(product)
)

let rec getAllPartialProductsHelper : aexp list * aexp list * int -> aexp list = (
	fun (l_orig, l_diff, upper_index) ->
		if upper_index < 0 then []
		else (
			let current_partial_product = 
				getPartialProduct(l_orig,l_diff,upper_index) in
			let prev_partial_products = 
				getAllPartialProductsHelper(l_orig,l_diff,upper_index-1) in
					current_partial_product::prev_partial_products
		)
)			

let getAllPartialProducts : aexp list * aexp list -> aexp list = (
	fun (l_orig, l_diff) ->
		let size = List.length l_orig in
		let result = getAllPartialProductsHelper(l_orig,l_diff,size - 1) in
			List.rev result
)

let rec diff : aexp * string -> aexp = fun (expr, var_diff) -> (
	match expr with
	| Const(i) -> zero
	| Var(var) -> (* var: string *)
		if var = var_diff then one
		else zero
	| Power(var, e) -> (* var: string, e : int *)
		if var <> var_diff then zero
		else ( (* var = var_diff *)
			match e with
			| 0 -> zero
			| 1 -> one
			| _ -> (* e * (var ^ (e-1) )*)
				simplifyAe(Times([Const(e);Power(var, e-1)]))
		)
	| Times(aexp_list_orig) -> (
			let aexp_list_diff = 
				List.map (fun aexp_elem -> diff(aexp_elem,var_diff)) aexp_list_orig in
			let to_sum = (
				getAllPartialProducts(aexp_list_orig,aexp_list_diff)
			) in
				simplifyAe(Sum(to_sum))
		)
	| Sum(aexp_list) -> (
		match aexp_list with
		| [] -> zero
		| h::[] -> diff(h, var_diff)
		| h::t -> ((* h: aexp, t: aexp list *)
			let first_diff : aexp = diff(h,var_diff) in
			let rest_diff : aexp = diff(Sum(t),var_diff) in
			let combined_diff = [first_diff;rest_diff] in
				simplifyAe(Sum(combined_diff))
		)
	)
)

(* 3. Test code *)
(*
(* ax^2 + bx + c *)
let testExpr1 = Sum([
	Times([
		Var("a");
		Power("x",2)
	]); (* ax^2 *)
	Times([
		Var("b");
		Var("x")
	]); (* bx *)
	Var("c") (* c *)
])
(**)
(* x^5 + 3 + bxc^2 *)
let testExpr2 = Sum([
	Sum([Const(3);Power("x",5)]); (* x^5 + 3*)
	Times([
		Var("b");
		Var("x");
		Power("c",2)
	]) (* bxc^2 *)
])
	
let rec printAe : aexp -> unit = fun expr ->
	match expr with
	| Const(n) -> Printf.printf "Const(%d)" n
	| Var(str) -> Printf.printf "Var(%s)" str
	| Power(str,n) -> Printf.printf "Power(%s,%d)" str n
	| Times(aexp_list) ->
		let sz = List.length aexp_list in
			(print_string "Times([";
			for i = 0 to sz - 1 do
				printAe(List.nth aexp_list i);
				if i <> (sz - 1) then print_string ";"
			done;
			print_string "])";
			)
	| Sum(aexp_list) ->
		let sz = List.length aexp_list in
			(print_string "Sum([";
			for i = 0 to sz - 1 do
				printAe(List.nth aexp_list i);
				if i <> (sz - 1) then print_string ";"
			done;
			print_string "])";
			)
(**)
let testRunner = fun (expr,var, msg) ->
	print_endline "====================================";
	print_endline("                           " ^ msg);
	print_endline "Expression:";
	printAe(expr);
	print_endline "\n----------";
	print_endline ("Diff by: " ^ var);
	print_endline "----------";
	let diff_result = diff(expr,var) in
		(print_endline "Result:";
		printAe(diff_result);
		print_newline())
(**)
let test =
	let expr = testExpr1 in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Var("x") in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Const(5) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Power("x",2) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Power("x",3) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Times([Const(3);Var("x");Var("a")]) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = testExpr2 in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = testExpr2 in
	let var = "d" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = testExpr2 in
	let var = "b" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = Var("x") in
	let var = "k" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = testExpr1 in
	let var = "a" in
	let msg = "Good" in
	testRunner(expr,var, msg)
let test =
	let expr = testExpr1 in
	let var = "c" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
*)