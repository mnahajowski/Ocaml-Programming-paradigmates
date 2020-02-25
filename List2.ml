(*1*)

let rec fibon(number) =
	match number with
	| 0 -> 0
	| 1 -> 1
	| _ -> fibon(number-2) + fibon(number-1);;

fibon(6);;


	
let rec fibon_iter(number, score1, score2) =
	match number with
	| 0 -> score1
	| 1 -> score2
	| _ -> fibon_iter(number-1, score2, score1+score2);;

let rec fibon2(number) = fibon_iter(number, 0, 1);;

fibon2(6);;


let rec fibon3(number) = 
	let rec fibon2_iter(number, score1, score2) =
	match number with
	| 0 -> score1
	| 1 -> score2
	| _ -> fibon2_iter(number-1, score2, score1+score2)
	in fibon2_iter(number, 0 ,1)
;;
	
fibon3(6);;

(*3*)

let root3 (a) = 
	let rec root3_iter(score) = 
		if abs_float (score ** 3. -. a) <= 1e-015 *. abs_float(a) then score
		else root3_iter(score +. (a /. score ** 2. -. score) /. 3.)
	in root3_iter(if a <= 1. then a else a/. 3.);;

root3(15.);;


(*6*)

let rec replaceNth(list1, number, changer) =
	match (list1, number) with
	| ([], _) -> []
	| (head :: tail, 0) -> changer :: tail
	| (head :: tail, _) -> head :: replaceNth(tail, number - 1, changer);;


(*5*)

let rec initSegment(firstList, secondList) =
	match(firstList, secondList) with
	| ([], _) -> true
	| (_, []) -> false
	| _ -> if List.hd firstList = List.hd secondList then initSegment(List.tl firstList, List.tl secondList)
					else false;;

	