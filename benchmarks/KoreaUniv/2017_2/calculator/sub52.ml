(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_helper : exp -> int list -> int
= fun e l ->
  match e with
  | X -> (match l with
          | [] -> raise (Failure "Value error: Can't find x value in the env")
          | [n] -> n
          | _ -> raise (Failure "Runtime error: unexpected env"))
  | INT n -> n
  | ADD (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 + v2
  | SUB (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 - v2
  | MUL (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 * v2
  | DIV (e1, e2) -> let v1 = calc_helper e1 l in
                    let v2 = calc_helper e2 l in
                    v1 / v2

  | SIGMA (e1, e2, exp) -> let n1 = calc_helper e1 l in
                           let n2 = calc_helper e2 l in
                           if n1 > n2 then 0
                           else (calc_helper exp [n1]) + (calc_helper (SIGMA ((ADD (e1, INT 1)), e2, exp)) l)

let calculator : exp -> int
= fun e ->
  calc_helper e []