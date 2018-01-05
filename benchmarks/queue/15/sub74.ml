module type Stack =
  sig
    type element
    type stack
    exception EMPTY_STACK
    val emptyStack: stack
    val pushStack: stack * element -> stack
    val popStack: stack -> element * stack
  end

module IntListStack =
  struct
    type element = int list
    type stack = element list
    exception EMPTY_STACK
    let emptyStack: stack = []
    let pushStack (stackToBePushed, newElement) =
      newElement::stackToBePushed
    let popStack stackToBePopped =
      match stackToBePopped with
      | [] -> raise EMPTY_STACK
      | top::restOfStack -> (top, restOfStack)
  end

module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    module S = IntListStack

    type element = int list
    type queue = (S.stack * S.stack)

    exception EMPTY_Q

    let emptyQ = (S.emptyStack, S.emptyStack)

    let enQ (queueToBeEnqueued, newElement) =
      match queueToBeEnqueued with
      | (pushQ, popQ) ->
        (IntListStack.pushStack(pushQ, newElement), popQ)

    let propagate stack1 =
      let rec clearPop stack1 stack2 =
        if stack1 = [] then stack2
        else
          let (e, s) = S.popStack stack1 in
          clearPop s (S.pushStack (stack2, e))
      in
      clearPop stack1 []

    let deQ (pushStack, popStack) =
      if popStack = S.emptyStack then
        let (s1, s2) = (S.emptyStack, propagate pushStack) in
        let (e, ns) = S.popStack s2 in
        (e, (s1, ns))
      else
        let (e, ns) = S.popStack popStack in
        (e, (pushStack, ns))
  end
