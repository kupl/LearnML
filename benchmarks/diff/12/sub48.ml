(* Name: Yoon Jae Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 2: Mathemadiga *)

(* 1. Provided declarations / definitions *)
type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list
(**)

(* 2. My code *)
let zero = CONST(0)
let one = CONST(1)

(* Helper function that simplifies an ae *)
let rec simplifyAe : ae -> ae = fun exp -> (
	match exp with
	| POWER(var,e) -> (
		match e with
		| 0 -> one
		| 1 -> VAR(var)
		| _ -> exp
	)
	| TIMES(ae_list) -> (
		match ae_list with
		| [] -> one
		| h::[] -> simplifyAe(h)
		| h::t -> (
			match h with
			| CONST(0) -> zero
			| CONST(1) -> simplifyAe(TIMES(t))
			| _ -> (
				let t_simp = simplifyAe(TIMES(t)) in
				let h_simp = simplifyAe(h) in (
					match (h_simp, t_simp) with
					| (CONST(0), _) -> zero
					| (_, CONST(0)) -> zero
					| (CONST(1), _) -> t_simp
					| (_, CONST(1)) -> h_simp
					| (_, _) -> TIMES([h_simp;t_simp])
				)
			)
		)
	)
	| SUM(ae_list) -> (
		match ae_list with
		| [] -> zero
		| h::[] -> simplifyAe(h)
		| h::t -> (
			match h with
			| CONST(0) -> simplifyAe(SUM(t))
			| _ -> (
				let t_simp = simplifyAe(SUM(t)) in
				let h_simp = simplifyAe(h) in (
					match (h_simp, t_simp) with
					| (CONST(0), _) -> t_simp
					| (_, CONST(0)) -> h_simp
					| (_, _) -> SUM([h_simp;t_simp])
				)
			)
		)
	)
	| _ -> exp
)

let rec removeIndex: ae list * int -> ae list = (
	fun (l, index_to_remove) ->
		if index_to_remove = 0 then List.tl l
		else (List.hd l)::(removeIndex(List.tl(l),index_to_remove - 1))
)

let getPartialProduct : ae list * ae list * int -> ae = (
	fun (l_orig, l_diff, index) ->
		let elem_diff : ae = List.nth l_diff index in
		let l_orig_without_index : ae list = removeIndex(l_orig,index) in
		let product : ae list = elem_diff::l_orig_without_index in
			TIMES(product)
)

let rec getAllPartialProductsHelper : ae list * ae list * int -> ae list = (
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

let getAllPartialProducts : ae list * ae list -> ae list = (
	fun (l_orig, l_diff) ->
		let size = List.length l_orig in
		let result = getAllPartialProductsHelper(l_orig,l_diff,size - 1) in
			List.rev result
)

let rec diff : ae * string -> ae = fun (expr, var_diff) -> (
	match expr with
	| CONST(i) -> zero
	| VAR(var) -> (* var: string *)
		if var = var_diff then one
		else zero
	| POWER(var, e) -> (* var: string, e : int *)
		if var <> var_diff then zero
		else ( (* var = var_diff *)
			match e with
			| 0 -> zero
			| 1 -> one
			| _ -> (* e * (var ^ (e-1) )*)
				simplifyAe(TIMES([CONST(e);POWER(var, e-1)]))
		)
	| TIMES(ae_list_orig) -> (
			let ae_list_diff = 
				List.map (fun ae_elem -> diff(ae_elem,var_diff)) ae_list_orig in
			let to_sum = (
				getAllPartialProducts(ae_list_orig,ae_list_diff)
			) in
				simplifyAe(SUM(to_sum))
		)
	| SUM(ae_list) -> (
		match ae_list with
		| [] -> zero
		| h::[] -> diff(h, var_diff)
		| h::t -> ((* h: ae, t: ae list *)
			let first_diff : ae = diff(h,var_diff) in
			let rest_diff : ae = diff(SUM(t),var_diff) in
			let combined_diff = [first_diff;rest_diff] in
				simplifyAe(SUM(combined_diff))
		)
	)
)

(* 3. Test code *)
(*
(* ax^2 + bx + c *)
let testExpr1 = SUM([
	TIMES([
		VAR("a");
		POWER("x",2)
	]); (* ax^2 *)
	TIMES([
		VAR("b");
		VAR("x")
	]); (* bx *)
	VAR("c") (* c *)
])
(**)
(* x^5 + 3 + bxc^2 *)
let testExpr2 = SUM([
	SUM([CONST(3);POWER("x",5)]); (* x^5 + 3*)
	TIMES([
		VAR("b");
		VAR("x");
		POWER("c",2)
	]) (* bxc^2 *)
])
	
let rec printAe : ae -> unit = fun expr ->
	match expr with
	| CONST(n) -> Printf.printf "CONST(%d)" n
	| VAR(str) -> Printf.printf "VAR(%s)" str
	| POWER(str,n) -> Printf.printf "POWER(%s,%d)" str n
	| TIMES(ae_list) ->
		let sz = List.length ae_list in
			(print_string "TIMES([";
			for i = 0 to sz - 1 do
				printAe(List.nth ae_list i);
				if i <> (sz - 1) then print_string ";"
			done;
			print_string "])";
			)
	| SUM(ae_list) ->
		let sz = List.length ae_list in
			(print_string "SUM([";
			for i = 0 to sz - 1 do
				printAe(List.nth ae_list i);
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
	let expr = VAR("x") in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = CONST(5) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = POWER("x",2) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = POWER("x",3) in
	let var = "x" in
	let msg = "Good" in
	testRunner(expr,var, msg)
(**)
let test =
	let expr = TIMES([CONST(3);VAR("x");VAR("a")]) in
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
	let expr = VAR("x") in
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