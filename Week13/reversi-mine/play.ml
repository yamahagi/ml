open Array 
open Color 
open Command 

type board = color array array 
(* http://tony-mooori.blogspot.jp/2015/10/aic.html からお借りしました。*)
let eval_board = [|
  [|68;-12;53;-8;-8;53;-12;68|];
  [|-12;-62;-33;-7;-7;-33;-62;-12|];
  [|53;-33;26;8;8;26;-33;53|];
  [|-8;-7;8;-18;-18;8;-7;-8|];
  [|-8;-7;8;-18;-18;8;-7;-8|];
  [|53;-33;26;8;8;26;-33;53|];
  [|-12;-62;-33;-7;-7;-33;-62;-12|];
  [|68;-12;53;-8;-8;53;-12;68|];
|]

let init_board () = 
  let board = Array.make_matrix 10 10 none in 
    for i=0 to 9 do 
      board.(i).(0) <- sentinel ;
      board.(i).(9) <- sentinel ;
      board.(0).(i) <- sentinel ;
      board.(9).(i) <- sentinel ;
    done;
    board.(4).(4) <- white;
    board.(5).(5) <- white;
    board.(4).(5) <- black;
    board.(5).(4) <- black;
    board 

let dirs = [ (-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1) ]

let flippable_indices_line board color (di,dj) (i,j) =
  let ocolor = opposite_color color in
  let rec f (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else 
      [] 
  and    g (di,dj) (i,j) r =
    if board.(i).(j) = ocolor then 
      g (di,dj) (i+di,j+dj) ( (i,j) :: r )
    else if board.(i).(j) = color then 
      r
    else 
      [] in 
    f (di,dj) (i,j) []
      
    

let flippable_indices board color (i,j) =
  let bs = List.map (fun (di,dj) -> flippable_indices_line board color (di,dj) (i+di,j+dj)) dirs in 
    List.concat bs 
    
let is_effective board color (i,j) =
  match flippable_indices board color (i,j) with 
      [] -> false
    | _  -> true 

let is_valid_move board color (i,j) =
  (board.(i).(j) = none) && is_effective board color (i,j) 


let doMove board com color =
  match com with 
      GiveUp  -> board
    | Pass    -> board
    | Mv (i,j) -> 
	let ms = flippable_indices board color (i,j) in 
	let _  = List.map (fun (ii,jj) -> board.(ii).(jj) <- color) ms in 
	let _  = board.(i).(j) <- color in 
	  board 
    | _ -> board 

let mix xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	     

let valid_moves board color = 
  let ls = [1;2;3;4;5;6;7;8] in 
  List.filter (is_valid_move board color)
    (mix ls ls)

let count board color = 
  let s = ref 0 in 
    for i=1 to 8 do 
      for j=1 to 8 do
        if board.(i).(j) = color then s := !s + 1 
      done
    done;
    !s

let count_both board =
(count board white) + (count board black)

let danger = [2; 7]
let safe = [1; 8]


let rec copy board =
let s = Array.map (fun x -> Array.copy x) board in
s

let rec max_eval_board ms  =
match ms with
|[] -> ((1,1),-2000)
|(a,b)::xs ->
let tmp_max = (max_eval_board xs) in
if (eval_board.(a-1).(b-1) >  (snd tmp_max))
then ((a,b),eval_board.(a-1).(b-1))
else tmp_max

(* 相手とそのあと自分が最善手を指した時のeval_boardの値、そしてその時の着手可能数を評価関数に組み込む*)

(*
let rec max_eval ms leng color board =
match ms with
        |[] -> ((1,1),-2000)
        |(a,b)::xs ->
        let opposite_col = (opposite_color color) in
        let copy_board = (copy board) in
         let newboard = (doMove copy_board (Mv (a,b)) color) in
         let opposite_ms = (valid_moves newboard opposite_col)  in
         let pre = (max_eval xs leng color board) in
         (match opposite_ms with
         |[] -> let max_value = eval_board.(a-1).(b-1) + 50+3*(leng) in
                if(max_value > (snd pre))
                then ((a,b),max_value)
                else
                pre
	 |(k,l)::xs ->
                let tmp_max = (snd (max_eval_board opposite_ms)) in
                let copy_board2 = (copy newboard) in
                let (c,d) = (fst (max_eval_board opposite_ms)) in
                let newboard2 = (doMove copy_board2 (Mv (c,d)) opposite_col) in
                let new_ms = (valid_moves newboard2 color)  in
                (match new_ms with
                        |[] -> let max_value = eval_board.(a-1).(b-1) - tmp_max -50 + 3*(leng - (List.length opposite_ms)) in
                                if(max_value > (snd pre))
                                then ((a,b),max_value)
                                else
                                pre
			|(ks,ls)::xss ->
                                let ((e,f),tmp_max2) =  (max_eval_board new_ms) in
                                let max_value = (eval_board.(a-1).(b-1) - tmp_max + tmp_max2)+ 3*(leng - (List.length opposite_ms) + (List.length new_ms)) in
                                if (max_value > (snd pre))
                                then
                                ((a,b),max_value)
                                else
                                pre))

*)
let rec to_last_sub board my_color color tmp_ms ms = 
let copy_board = (copy board) in
let opposite = (opposite_color color) in
if (count_both board) = 64 then
	if (count board my_color) > (count board (opposite_color my_color)) 
	then (1,0)
	else (0,1)
else 
match ms with
|[] ->
let new_valid = (valid_moves board opposite) in
	(match new_valid with
	|[] -> if (count board my_color) > (count board (opposite_color my_color))
        then (1,0)
        else (0,1)
	|(k,q)::xs -> (to_last_sub board my_color opposite new_valid new_valid))
|(xa,xb)::xc ->
	(match tmp_ms with
	|[] -> (0,0) 
	|(a,b)::xs -> 
	let new_board = (doMove (copy_board) (Mv(a,b)) color) in
	let new_valid = (valid_moves new_board opposite) in
	let (a1,b1) = (to_last_sub new_board my_color opposite new_valid new_valid) in
	let (a2,b2) = (to_last_sub board my_color color xs ms) in
	(a1+a2,b1+b2))
(* 相手が二回目動かした後のリストを返す *)
(*引数は自分が動かして相手が動かしたあとのボード、自分、自分、自分が動かせる範囲、自分が動かせる範囲*)

(*
let rec to_last_forth board my_color color tmp_ms ms =
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> let new_valid = (valid_moves copy_board opposite) in
        (match new_valid with
        |[] -> if (count board my_color) > (count board (opposite_color my_color))
        then [(1,0)]
        else [(0,1)]
        |(k,q)::xs -> [(to_last_sub board my_color opposite new_valid new_valid)])
|(xa,xb)::xc ->
        (match tmp_ms with
	|[] -> []
	|(a,b)::xs ->
	let new_board = (doMove copy_board (Mv(a,b)) color) in
	let new_valid = (valid_moves new_board opposite) in
	(to_last_sub new_board color opposite new_valid new_valid)::(to_last_forth board color color xc ms))

(*自分が動かす->相手が動かす->その手を打ったあと黒がまた打ちそのあと白の手によって結果がどうなるかのリストをto_last_forthで作り、それをappendで連結*)
(*引数は自分が動かしたあとのボード、自分、相手、相手が動かせる範囲、相手が動かせる範囲*)
let rec to_last_third board my_color color tmp_ms ms =
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> let new_valid = (valid_moves copy_board opposite) in
        (match new_valid with
        |[] -> if (count board my_color) > (count board (opposite_color my_color))
        then [(1,0)]
        else [(0,1)]
        |(k,q)::xs -> [(to_last_sub board my_color opposite new_valid new_valid)])
|(xa,xb)::xc ->
        (match tmp_ms with
        |[] -> []
        |(a,b)::xs ->
        let new_board = (doMove copy_board (Mv(a,b)) color) in
        let new_valid = (valid_moves new_board opposite) in
        (to_last_forth new_board my_color opposite new_valid new_valid)@(to_last_third board my_color color xs ms))


(* to_lastの次の局面において相手が動かした場合の自分の勝つ場合の合計のリストを返す*)
let rec to_last_second board my_color color tmp_ms ms =
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> let new_valid = (valid_moves copy_board opposite) in
        (match new_valid with
        |[] -> if (count board my_color) > (count board (opposite_color my_color))
        then [(1,0)]
        else [(0,1)]
        |(k,q)::xs -> [(to_last_sub board my_color opposite new_valid new_valid)])
|(xa,xb)::xc ->
	(match tmp_ms with
	|[] -> []
	|(a,b)::xs ->
	let new_board = (doMove copy_board (Mv(a,b)) color) in
	let new_valid = (valid_moves new_board opposite) in
	(to_last_sub new_board my_color opposite new_valid new_valid)::(to_last_second board my_color color xs ms))
*)

(* 自分が動かしてそのあと相手が動かしたあと自分がどれくらい勝つ場合があるかのリストのリストと何を動かしたかのタプルを得る*)
(*
let rec to_last board color ms=
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> []
|(a,b)::xs -> 
let new_board = (doMove copy_board (Mv(a,b)) color) in
let new_valid = (valid_moves new_board opposite) in
((a,b),(to_last_second new_board color opposite new_valid new_valid))::(to_last board color xs)
*)


(* to_lastの次の局面において相手が動かした場合の自分の勝つ場合の合計のリストを返す*)
let rec to_last_second board my_color color tmp_ms ms =
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> let new_valid = (valid_moves copy_board opposite) in
        (match new_valid with
        |[] -> if (count board my_color) > (count board (opposite_color my_color))
        then [(1,0)]
        else [(0,1)]
        |(k,q)::xs -> [(to_last_sub board my_color opposite new_valid new_valid)])
|(xa,xb)::xc ->
	(match tmp_ms with
	|[] -> []
	|(a,b)::xs ->
	let new_board = (doMove copy_board (Mv(a,b)) color) in
	let new_valid = (valid_moves new_board opposite) in
	(to_last_sub new_board my_color opposite new_valid new_valid)::(to_last_second board my_color color xs ms))

(* 自分が動かしてそのあと相手が動かしたあと自分がどれくらい勝つ場合があるかのリストのリストと何を動かしたかのタプルを得る*)
let rec to_last board color ms=
let copy_board = (copy board) in
let opposite = (opposite_color color) in
match ms with
|[] -> []
|(a,b)::xs -> 
let new_board = (doMove copy_board (Mv(a,b)) color) in
let new_valid = (valid_moves new_board opposite) in
((a,b),(to_last_second new_board color opposite new_valid new_valid))::(to_last board color xs)

(*resultの中から要素に0が入っていない(負け確定の場合がない)リストのリストを返す*)
let rec not_zero_in result =
match result with
|[] -> []
|(a,x)::xs -> if (not (List.exists (fun (k,l) -> k=0) x)) then
	(a,x)::(not_zero_in xs)
	else
	(not_zero_in xs)

let rec sum_list suma= 
match suma with
|[] -> (0,0)
|(a,b)::xs -> ((a + (fst (sum_list xs))),(b+(snd (sum_list xs))))

(* 最大の合計勝ちパターンがあるもののMvとその合計値を返す*)
let rec maximum result_list =
match result_list with
|[] -> (Mv(0,0),(0,1))
|((tmp_a,tmp_b),x)::xs -> 
	let (sum_a,sum_b) = (sum_list x) in
	let (max_a,max_b) = (snd (maximum xs)) in
	if (sum_a*max_b) >= (sum_b*max_a) then
	(Mv(tmp_a,tmp_b),(sum_a,sum_b))
	  else (maximum xs)
	  

(* to_lastの結果を判断する。
基本的に自分がその手を打った後相手の選ぶ手によって自分の勝ちパターンが0になる可能性があるならばその手は選ばない。
そしてそうならない選択肢の中で、最も勝ちパターンの合計が多い選択肢を選ぶ。
全ての選択肢に勝ちパターン0のものが含まれるのであれば、普通に最も勝ちパターンんの多いものを選ぶ。*)
let rec to_last_judge board color ms =
let result = to_last board color ms in
match result with
|[] -> (Pass,(0,0))
|a::xs -> 
	let not_zero = (not_zero_in result) in
	(match not_zero with
	|[] -> (maximum result)
	|x::xs -> (maximum not_zero))

let rec opposite_ms color board a b =
	let opposite_col = (opposite_color color) in
        let copy_board = (copy board) in
        let newboard = (doMove copy_board (Mv (a,b)) color) in
        let opposite = (valid_moves newboard opposite_col)  in
	 opposite

let rec max_eval ms leng color board =
  match ms with
        |[] -> ((1,1),-2000)
        |(a,b)::xs ->
        let opposite_col = (opposite_color color) in
        let copy_board = (copy board) in
         let newboard = (doMove copy_board (Mv (a,b)) color) in
         let opposite_ms = (valid_moves newboard opposite_col)  in
         let pre = (max_eval xs leng color board) in
           (match opposite_ms with
         |[] -> let max_value = eval_board.(a-1).(b-1) + 50+3*(leng) in
                if(max_value > (snd pre))
                then ((a,b),max_value)
                  else
                  pre
         |(k,l)::xs ->
                let ((c,d),tmp_max) = (max_eval_board opposite_ms) in
                let copy_board2 = (copy newboard) in
                let (c,d) = (fst (max_eval_board opposite_ms)) in
                let newboard2 = (doMove copy_board2 (Mv (c,d)) opposite_col) in
                let new_ms = (valid_moves newboard2 color)  in
                  (match new_ms with
			|[] -> let max_value = eval_board.(a-1).(b-1) - tmp_max -50 + 3*(leng - (List.length opposite_ms)) in
                                if(max_value > (snd pre))
                                then ((a,b),max_value)
                                  else
                                  pre
                        |(ks,ls)::xss ->
			let (ca,da) = (fst (max_eval_board new_ms)) in
			let my_max2 = (snd (max_eval_board new_ms)) in
                	let copy_board3 = (copy newboard2) in
                	let newboard3 = (doMove copy_board3 (Mv (ca,da)) color) in
                	let opposite_ms2 = (valid_moves newboard3 opposite_col)  in
			 (match opposite_ms2 with
				|[] -> let max_value = eval_board.(a-1).(b-1) - tmp_max + my_max2 +50 + 3*(leng - (List.length opposite_ms)+ (List.length new_ms)) in
                                if(max_value > (snd pre))
                                then ((a,b),max_value)
                                  else
                                  pre
				|(s,p)::msp ->
				let ((ci,di),opposite_max2) = (max_eval_board opposite_ms2) in
                        	let copy_board4 = (copy newboard3) in
				let newboard4 = (doMove copy_board4 (Mv (ci,di)) opposite_col) in
                        	let my_ms = (valid_moves newboard4 color)  in
                           	
				 	(match my_ms with
					|[] ->
					let max_value = (eval_board.(a-1).(b-1) - tmp_max + my_max2 - opposite_max2)+ 3*(leng - (List.length opposite_ms) + (List.length new_ms) - (List.length opposite_ms2)) in
                                		if (max_value > (snd pre))
                                		then
                               			((a,b),max_value)
                                		else
                                		pre
					|(mya,myb)::myxs ->
					let ((mya2,myb2),my_max3) = (max_eval_board my_ms) in
                                	let copy_board5 = (copy newboard4) in
                                	let newboard5 = (doMove copy_board5 (Mv (mya2,myb2)) color) in 
	                                let opposite_ms3 = (valid_moves newboard4 opposite_col)  in
					let max_value = (eval_board.(a-1).(b-1) - tmp_max + my_max2 - opposite_max2 + my_max3)+ 3*(leng - (List.length opposite_ms) + (List.length new_ms) - (List.length opposite_ms2) + (List.length my_ms)) in
                                                if (max_value > (snd pre))
                                                then
                                                ((a,b),max_value)
                                                else
                                                pre))))



let play board color =
  let ms = valid_moves board color in
    match ms with
    |[] ->
      Pass
    |(x1,x2)::xs ->
        let leng = (List.length ms) in
        if leng=1 then Mv(x1,x2)
        else
	(if (count_both board) <= 54 then
        let (a,b) = (fst (max_eval ms leng color board)) in
      (Mv (a,b))
	else
	let command = (fst (to_last_judge board color ms)) in
	command
	)

let print_board board = 
  print_endline " |A B C D E F G H ";
  print_endline "-+----------------";
  for j=1 to 8 do 
    print_int j; print_string "|";
    for i=1 to 8 do 
      print_color (board.(i).(j)); print_string " " 
    done;
    print_endline ""
  done;
  print_endline "  (X: Black,  O: White)"
      

let report_result board = 
  let _ = print_endline "========== Final Result ==========" in 
  let bc = count board black in 
  let wc = count board white in 
    if bc > wc then 
      print_endline "*Black wins!*" 
    else if bc < wc then 
      print_endline "*White wins!*" 
    else
      print_endline "*Even*";
    print_string "Black: "; print_endline (string_of_int bc);
    print_string "White: "; print_endline (string_of_int wc);
    print_board board 
