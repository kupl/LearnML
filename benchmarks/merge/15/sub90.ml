(*
 * SNU 4190.310 Programming Languages (Fall 2014)
 *
 * SM5
 *)
open K
open Sm5
module Translator = struct

let counter =
  let count = ref (-1) in
    fun () -> incr count; "!@#^" ^ string_of_int(!count)

let rec trans : K.program -> Sm5.command
= fun pgm ->
    let rec k_num n =
       [(Sm5.PUSH (Sm5.Val (Sm5.Z n)))] in
    let rec k_true =
        [(Sm5.PUSH (Sm5.Val (Sm5.B true)))] in
    let rec k_false =
        [(Sm5.PUSH (Sm5.Val (Sm5.B false)))] in
    let rec k_unit =
        [(Sm5.PUSH (Sm5.Val (Sm5.Unit)))] in
    let rec k_add exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.ADD]) in

    let rec k_sub exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.SUB]) in

    let rec k_write exp =
        List.append (trans exp)  [Sm5.BIND "__K_WRITE#"; Sm5.PUSH(Sm5.Id "__K_WRITE#"); Sm5.PUSH(Sm5.Id "__K_WRITE#");  (Sm5.PUT)] in

    let rec k_mul exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.MUL]) in

    let rec k_div exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.DIV]) in

    let rec k_eq exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.EQ]) in

    let rec k_less exp1 exp2 =
        List.append (trans exp1) (List.append (trans exp2) [Sm5.LESS]) in

    let rec k_not exp =
        List.append (trans exp) [Sm5.NOT] in

    let rec k_seq exp1 exp2 =
        List.append (trans exp1) (List.append [Sm5.POP] (trans exp2)) in

    let rec k_letv id exp1 exp2 =
        let list1 = [
            Sm5.MALLOC;
            Sm5.BIND "__K_LETV#";
            Sm5.PUSH(Sm5.Id "__K_LETV#");
            Sm5.BIND id
            ] in
        let list2 = trans exp1 in
        let list3 = [Sm5.PUSH(Sm5.Id "__K_LETV#");
            Sm5.STORE] in
        let list4 = trans exp2 in
            List.append list1 (List.append list2 (List.append list3 list4)) in

    let rec k_var id =
        [Sm5.PUSH(Sm5.Id id); Sm5.LOAD] in

    let rec k_assigin id exp =
        let list1 = trans exp in
        let list2 = [Sm5.PUSH(Sm5.Id id); Sm5.STORE; Sm5.PUSH(Sm5.Id id); Sm5.LOAD] in
            List.append list1 list2 in

    let rec k_read id =
        [Sm5.GET; Sm5.BIND "__K_READ#"; Sm5.PUSH(Sm5.Id "__K_READ#"); Sm5.PUSH(Sm5.Id id); Sm5.STORE; Sm5.PUSH(Sm5.Id "__K_READ#")] in

    let rec k_if exp1 exp2 exp3 =
        let list1 = trans exp1 in
        let list2 = [Sm5.JTR ((trans exp2), (trans exp3))] in
            List.append list1 list2 in

    let rec k_letf id1 id2 exp1 exp2 =
        let list1 = [Sm5.PUSH(Sm5.Fn (id2, (List.append [Sm5.BIND id1] (trans exp1)))); Sm5.BIND id1] in
        let list2 = (trans exp2) in
            List.append list1 list2 in


    let rec k_callv id exp =
        let list1 = [Sm5.PUSH(Sm5.Id id); Sm5.PUSH(Sm5.Id id)] in
        let list2 = (trans exp) in
        let list3 = [Sm5.MALLOC; Sm5.CALL] in
            List.append list1 (List.append list2 list3) in

    let rec k_callr id1 id2 =
        let list1 = [Sm5.PUSH(Sm5.Id id1); Sm5.PUSH(Sm5.Id id1); Sm5.PUSH(Sm5.Id id2); Sm5.LOAD; Sm5.PUSH(Sm5.Id id2); Sm5.CALL] in
            list1 in

    let rec k_while exp1 exp2 =
        let fun_name = String.concat "#" ["__K_WHILE_FUN"; counter()] in
        let while_fun_body = K.IF (exp1, K.SEQ(exp2, (K.CALLV (fun_name, (K.VAR "__K_WHILE_PARAM#")))), K.UNIT) in
        let while_run = K.CALLV (fun_name, (K.UNIT)) in
        let k_while_function = (k_letf fun_name "__K_WHILE_PARAM#" while_fun_body while_run) in
            k_while_function in

    let rec k_for id exp1 exp2 exp3 =
    let k =
  K.LETV(
    "__FOR_END#",
    exp2,
    K.SEQ(
      K.ASSIGN(
        id, exp1
      ),
      K.WHILE(
        K.LESS(
          K.VAR(
            id
          ),
          K.ADD(
            K.VAR(
              "__FOR_END#"
            ),
            K.NUM(
              1
            )
          )
        ),
        K.SEQ(
          exp3,
          K.ASSIGN(
            id,
            K.ADD(
              K.VAR(
                id
              ),
              K.NUM(
                1
              )
            )
          )
        )
      )
    )
  )

in (trans k) in
    match pgm with
    | K.NUM n -> (k_num n)
    | K.TRUE -> (k_true)
    | K.FALSE -> (k_false)
    | K.UNIT -> (k_unit)
    | K.ADD (exp1, exp2) -> (k_add exp1 exp2)
    | K.SUB (exp1, exp2) -> (k_sub exp1 exp2)
    | K.MUL (exp1, exp2) -> (k_mul exp1 exp2)
    | K.DIV (exp1, exp2) -> (k_div exp1 exp2)
    | K.EQUAL (exp1, exp2) -> (k_eq exp1 exp2)
    | K.LESS (exp1, exp2) -> (k_less exp1 exp2)
    | K.NOT exp -> (k_not exp)
    | K.SEQ (exp1,  exp2) -> (k_seq exp1 exp2)
    | K.LETV (id, exp1, exp2) -> (k_letv id exp1 exp2)
    | K.VAR id -> (k_var id)
    | K.ASSIGN (id, exp) -> (k_assigin id exp)
    | K.WRITE exp -> k_write exp
    | K.READ id -> k_read id
    | K.IF (exp1, exp2, exp3) -> (k_if exp1 exp2 exp3)
    | K.LETF (id1, id2, exp1, exp2) -> (k_letf id1 id2 exp1 exp2)
    | K.CALLV (id, exp) -> (k_callv id exp)
    | K.CALLR (id1, id2) -> (k_callr id1 id2)
    | K.WHILE (exp1, exp2) -> (k_while exp1 exp2)
    | K.FOR (id, exp1, exp2, exp3) ->  (k_for id exp1 exp2 exp3)
    (*
    (k_for id exp1 exp2 exp3)
    (k_for "__Y" (NUM 10) (NUM 20)
          (WRITE(
            (VAR "__y__")
          )))*)
end
