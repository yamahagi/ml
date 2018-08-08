type tyvar = int
	      
type ty =
  | TyInt 
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyCons of ty*ty
  | TyNil
  | TyPair of ty*ty
  | TyList of ty

type type_schema
=
tyvar list * ty

(*
 * Generate a fresh type variable
 *   (i.e. a type variable that has not been appeared)
 *)
let i=ref 0
let new_tyvar () = 
i:=!i+1;
!i;;

let rec print_type t =
	match t with
	|TyInt -> print_string "int"
	|TyBool -> print_string "bool"
	|TyFun(a,b) -> print_string "(";
			print_type a;
			print_string "->";
			print_type b;
			print_string")" 
	|TyVar(a) -> print_string ("a"^(string_of_int a))
