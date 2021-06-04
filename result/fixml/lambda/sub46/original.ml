type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let rec check : lambda -> bool =
 fun a ->
  match a with
  | V q -> false
  | P (q, p) -> (
      match p with
      | V x -> if q = x then true else false
      | P (x, y) -> (
          match y with
          | P (a, b) -> check (P (a, b)) || check (P (x, b)) || check (P (q, b))
          | V a -> a = x || a = q
          | C (a, b) -> (
              match (a, b) with
              | V a, V b -> (a = q || a = x) && (b = q || b = x)
              | V a, P (c, d) ->
                  (a = q || a = x)
                  && (check (P (c, d)) || check (P (x, d)) || check (P (q, d)))
              | P (a, b), V c ->
                  (check (P (a, b)) || check (P (x, b)) || check (P (q, b)))
                  && (c = q || c = x)
              | P (a, b), P (c, d) ->
                  (check (P (a, b)) || check (P (x, b)) || check (P (q, b)))
                  && (check (P (c, d)) || check (P (x, d)) || check (P (q, d)))
              | _, _ -> check a && check b ) )
      | C (x, y) -> (
          match (x, y) with
          | V a, V b -> a = q && b = q
          | V a, P (c, d) -> a = q && (check (P (c, d)) || check (P (q, d)))
          | P (a, b), V c -> (check (P (a, b)) || check (P (q, b))) && c = q
          | P (a, b), P (c, d) ->
              (check (P (a, b)) || check (P (q, b)))
              && (check (P (c, d)) || check (P (q, d)))
          | _, _ -> check x && check y ) )
  | C (q, p) -> check q && check p

(*
let _= 
let print_bool x = print_endline (string_of_bool x) in 

let a81 = check (P("a", V "a")) in 
let a82 = check (P("a", P("a", V "a"))) in 
let a83 = check (P("a", P("b", C(V "a", V "b")))) in 
let a84 = check (P("a", C(V "a", P("b", V "a")))) in 
let a85 = check (P("a", V "b")) in 
let a86 = check (P("a", C(V "a", P("b", V "c")))) in 
let a87 = check (P("a", P("b", C(V "a", V "c")))) in 

print_bool(false = check ( V "a")); 
print_bool(true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a")))))); 
print_bool(false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c")))))); 
print_bool(true = a81); 
print_bool(true = a82); 
print_bool(true = a83); 
print_bool(true = a84); 
print_bool(false = a85); 
print_bool(false = a86); 
print_bool(false = a87);;




*)
