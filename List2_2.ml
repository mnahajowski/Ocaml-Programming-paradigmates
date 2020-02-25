let rec listLength(myList1) = 
	if myList1 = [] then 0
	else 1 + listLength(List.tl myList1);;

listLength([1;2;3;4;5;2]);;

let rec listAppend(myList1, myList2) =
	if myList1 = [] then myList2
	else if myList2 = [] then myList1
	else List.hd myList1 :: List.hd myList2 :: listAppend(List.tl myList1, List.tl myList2);;

listAppend([5;4;3;2],[1;2;3;4;5;6;10]);;

