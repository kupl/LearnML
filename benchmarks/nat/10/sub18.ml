exception Error of string
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat =
  fun input ->
    try (match (snd input)
         with ZERO -> (fst input)
           | SUCC(x) -> natadd(SUCC(fst input),x)
    ) with _ -> raise (Error "cake is a lie")
let natmul : nat*nat -> nat =
  fun input ->
    let rec natadd : nat * nat -> nat =
      fun add_input -> match (snd add_input)
      with ZERO -> (fst add_input)
        | SUCC(x) -> natadd(SUCC(fst add_input),x)
    in
    let rec natmul : nat * nat -> nat =
      fun mul_input -> match (snd mul_input)
      with ZERO -> (fst mul_input)
        | SUCC(x) -> natmul (natadd((fst input), (fst mul_input)), x)
    in
      try (
        natmul (ZERO, (snd input))
      ) with _ -> raise (Error "cake is a lie")
