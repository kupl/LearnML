
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat =
  fun (n, m) ->
    match m with
    | ZERO -> n
    | SUCC m_pre -> natadd (SUCC n, m_pre)

and natmul : nat * nat -> nat =
  fun (n, m) ->
    match m with
    | ZERO -> ZERO
    | SUCC m_pre -> natadd (natmul (n, m_pre), n)
