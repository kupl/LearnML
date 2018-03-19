
exception NotImplemented
exception UndeterminedVariableX
exception InappropirateRange

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  type env = Env of int list

  let length = fun x -> match x with
                        |Env(a) -> List.length a
  let first = fun x -> match x with
                        | Env(a) -> (match a with
                                |[] -> raise NotImplemented
                                |hd::tl -> hd)

  let rec envCalculator: env * exp -> int
  = fun (env, exp) -> match exp with
  | X -> if (length env) = 0 then raise UndeterminedVariableX else first env
  | INT(a) -> a
  | ADD(a,b) -> envCalculator(env, a) + envCalculator(env, b)
  | SUB(a,b) -> envCalculator(env, a) - envCalculator(env, b)
  | MUL(a,b) -> envCalculator(env, a) * envCalculator(env, b)
  | DIV(a,b) -> envCalculator(env, a) / envCalculator(env, b)
  | SIGMA(a,b,c) -> let trueA = envCalculator(env,a) in
                        let trueB = envCalculator(env,b) in
                        if trueA > trueB then raise InappropirateRange else
                                if trueA = trueB then envCalculator(Env([trueA]), c)
                                else envCalculator(Env([trueA]), c) + envCalculator(env, SIGMA(ADD(INT(1),a),b,c))

  let calculator : exp -> int
  = fun exp -> envCalculator(Env([]), exp)