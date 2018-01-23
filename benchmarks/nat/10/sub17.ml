exception Error of string
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat =
  fun input ->
    (match (snd input)
         with ZERO -> (fst input)
           | SUCC(x) -> natadd(SUCC(fst input),x)
    )
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
        natmul (ZERO, (snd input))
