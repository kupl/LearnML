(* 2006-11782 Song Young-chan, Hw1-5 Natural Number *)

type nat = ZERO | SUCC of nat

let rec natadd((first:nat),(second:nat)) =
	match (first,second) with
	 (ZERO,result)|(result,ZERO) -> result
	|(first,SUCC(remain)) -> natadd (SUCC(first),remain)

let rec natmul((first:nat),(second:nat)) =
	match (first,second) with
	 (ZERO,result)|(result,ZERO) -> ZERO
	|(first,SUCC(ZERO)) -> natadd(first,ZERO)
	|(first,SUCC(remain)) -> natadd(natmul(first,remain),first)
