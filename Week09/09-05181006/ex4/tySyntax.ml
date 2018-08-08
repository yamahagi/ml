type tyvar = int
	      
type ty =
  | TyInt 
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty*ty
  | TyList of ty

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
                        print_string")";
                        print_string"\n"
        | TyPair (ty1,ty2) -> print_type ty1; print_string " * "; print_type ty2
        | TyList ty -> (match ty with |(TyVar newvar) -> print_string"[]"
                        |ty1 -> print_type ty1; print_string " list")
        | TyVar a -> print_string ("a"^(string_of_int a))
