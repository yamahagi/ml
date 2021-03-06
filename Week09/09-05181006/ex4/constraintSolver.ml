open TySyntax

exception TyError

(*
 * the type of substitution
 *)

type subst = (tyvar * ty) list

(*
 * the type of constraints
 *   a list of equations t1 = t2 for types t1 and t2
 *)
type constraints = (ty * ty) list

(*
 * return the most general unifier of the constraints
 * raise TyError if unification fails
 *)

(*
 * apply the substitution to the type
 *)

let rec ty_lookup ty subst =
  try List.assoc ty subst with Not_found -> TyVar ty

let rec ty_subst subst ty =
match ty with
|TyInt -> TyInt
|TyBool ->TyBool
|TyFun(t1,t2) -> TyFun((ty_subst subst t1),(ty_subst subst t2))
|TyVar a -> (ty_lookup  a subst)
|TyPair (t1, t2) -> TyPair (ty_subst subst t1, ty_subst subst t2)
|TyList t1 -> ty_subst subst t1

(*subst1(subst2(x))*)
let rec compose subst1 subst2 =
 match subst2 with
 |[] -> []
 |(x1,x2)::x3 ->
        (match x2 with
        |TyVar v1 ->( (x1,(ty_subst subst1 x2))::(compose subst1 x3))
        |TyFun (v1,v2) ->((x1,(ty_subst subst1 x2))::(compose subst1 x3))
        |_ -> (x1,x2)::(compose subst1 x3))


let rec ty_con s c =
  match c with
  | [] -> []
  | (t1, t2) :: x -> (ty_subst s t1, ty_subst s t2) :: (ty_con s x)


let rec ty_unify const = 
	match const with
	|[]->[]
	|(ty1,ty2)::const1 ->
		match (ty1,ty2) with
		|(TyInt,TyInt) |(TyBool,TyBool) -> (ty_unify const1)
		|(TyFun(ty1,ty2),TyFun(ty3,ty4))-> (ty_unify ([(ty1,ty3);(ty2,ty4)]@const1))
		|(TyVar(t1),ty)|(ty,TyVar(t1)) -> 
		if (TyVar(t1) = ty) then (ty_unify const1)
		else
		if (ty_subst ([(t1, ty)]) ty) = ty then 
		(compose [(t1, ty)]  (ty_unify (ty_con ([(t1, ty)]) const1))) 
		else raise TyError
		|(TyPair(a,b),TyPair(c,d)) -> (ty_unify ([(a,c);(b,d)]@const1))
                |((TyList a),(TyList b)) -> (ty_unify ([a,b]@const1))
		|(_,_) ->raise TyError		

