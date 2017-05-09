

type peano = Z|S of peano;;

let rec add x y = match y with
	Z-> x
	|S yy -> S(add x yy);;

let rec sub x y = match (x, y) with 
	(xx, Z)-> xx
	|(Z, yy) -> Z
	|(S xx, S yy)-> sub xx yy;;

let rec mul x y = match (x, y)  with
	(xx, Z) -> Z
	|(Z, yy) -> Z
	|(xx, S yy) -> add (mul xx yy) (xx);;

let rec power x y = match (x, y) with
	(xx, Z) -> S Z
	|(Z, xx) -> Z
	|(S Z, xx) -> S Z
	|(xx, S yy) -> mul (power xx yy) (xx);;

let peano_of_int n =
	 let rec make_from_int n p_n =
	 	 match n with
		 	0 ->p_n
			| x -> S(make_from_int (x - 1) p_n) in
	  make_from_int n Z;;

let print_peano p_n =
	let rec make_from_peano p_n str =
		match p_n with
			Z -> "0"^str
			|S x -> make_from_peano x "'"^str in
	make_from_peano p_n "";;

let inc p_n = add(p_n)(S Z);;

let rec div x y = match sub (inc (x)) (y)  with
        Z -> Z
        |_ -> add (S Z) (div  (sub x y) (y));;


let rec int_of_peano p = match p with
       Z -> 0
       | S x -> 1 + int_of_peano x;;

let dec p_n = 
 	match p_n with
		Z -> Z
		|_ -> sub(p_n)(S Z);;


