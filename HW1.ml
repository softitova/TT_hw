
type peano = Z|S of peano;;

let rec add x y = match y with
	Z-> x
	|S yy -> S(add x yy);;

let rec sub x y = match (x, y) with 
	(x, Z)-> x
	|(Z, y) -> Z
	|(S x, S y)-> sub x y;;

let rec mul x y = match (x, y)  with
	(x, Z) -> Z
	|(Z, y) -> Z
	|(x, S y) -> add (mul x y) (x);;

let peano_from_int n =
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

let inc p_n = add(p_n)(peano_from_int 1);;

let dec p_n = 
 	match p_n with
		Z -> Z
		|_ -> sub(p_n)(peano_from_int 1);;

let e = print_peano (add (peano_from_int 1) (peano_from_int 1));;
let a = print_peano (inc (peano_from_int 2));;
let b = print_peano (sub (peano_from_int 3) (peano_from_int 1));;
let c = print_peano (dec (peano_from_int 0));;
let d = print_peano (mul (peano_from_int 2) (peano_from_int 3));;

print_string d;;

