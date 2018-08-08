type name = string
type value =
| VInt of int
| VBool of bool
| VFun of name * expr * env
| VRecFun of name * name * expr * env | VPair of value * value
| VNil
| VCons of value * value
and env = (name * value) list

type pattern = PInt of int | PBool of bool
             | PVar of name
             | PPair of pattern * pattern
             | PNil | PCons of pattern * pattern

exception MisMatch

 type 'a option =
  | None
  | Some of 'a;;

let rec find_match pattern value =
match (pattern,value) with
|(PInt a,VInt b)when(a=b)-> Some []
|(PBool a,VBool b)when(a=b)-> Some []
|(PVar a,b)->Some [(a,b)]
|(PPair(a,b),VPair(c,d)) |(PCons (a,b),VCons(c,d))->
(match ((find_match a c),(find_match b d)) with
	|(Some e,Some f)->
	Some (e@f@[])
	|_ -> None)
|(PNil,VNil)->Some []
|(_,_)->None

