
let x1 = 7.5;;

((let x = x1 +. x1 and y = 2.0 in 
	x +. x +.
	(let x = 10.0 in x +. y)
	) +. 1.0);;

let x2 = 5;;
let k3 = (3+4,2., 2 < 4)
let k3_1 (x,y,z) = x;;
k3_1 k3;;