let board = [|
    [|3;3;3;3;3;3;3;3;3;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;2;1;0;0;0;3|];
    [|3;0;0;0;2;2;0;0;0;3|];
    [|3;0;0;0;2;2;2;0;0;3|];
    [|3;0;0;0;0;0;0;1;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;3;3;3;3;3;3;3;3;3|];|];;

let board2 = [|
    [|3;3;3;3;3;3;3;3;3;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;2;0;0;0;0;3|];
    [|3;0;0;0;2;2;0;0;0;3|];
    [|3;0;0;0;2;1;2;0;0;3|];
    [|3;0;0;0;0;0;1;0;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;0;0;0;0;0;0;0;0;3|];
    [|3;3;3;3;3;3;3;3;3;3|];|];;
let init = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|]; 
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 1; 2; 0; 0; 0; 3|]; 
  [|3; 0; 0; 0; 2; 1; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|]; 
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|]; 
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]

なぜかH6を送る
let board3 = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|]; 
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 1; 2; 0; 0; 0; 3|]; 
  [|3; 0; 0; 0; 2; 1; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 2; 2; 2; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 0; 0; 0; 0; 0; 0; 0; 0; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 1; 2; 2; 0; 1; 1; 1; 0; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 0; 0; 3|]; 
  [|3; 1; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 1; 2; 1; 1; 1; 3|]; 
  [|3; 0; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 0; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 0; 0; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board1 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 1; 1; 1; 0; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 1; 2; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 0; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 0; 0; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board2 = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 1; 2; 2; 2; 1; 1; 1; 0; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|]; 
  [|3; 1; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 1; 2; 1; 1; 1; 3|]; 
  [|3; 0; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 0; 2; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board2_1 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 1; 1; 1; 0; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 2; 1; 1; 2; 1; 1; 1; 3|];
  [|3; 2; 2; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 2; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 0; 2; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board2_2 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 1; 1; 1; 0; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 2; 1; 1; 2; 1; 1; 1; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 0; 2; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3 = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|]; 
  [|3; 1; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 1; 2; 1; 1; 1; 3|]; 
  [|3; 0; 1; 1; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_1 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 1; 1; 1; 1; 2; 2; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_2 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 2; 2; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_3 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 2; 1; 1; 2; 2; 1; 2; 3|];
  [|3; 2; 2; 2; 2; 2; 1; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_4 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 1; 2; 3|];
  [|3; 1; 1; 2; 1; 2; 2; 1; 2; 3|];
  [|3; 0; 2; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 2; 2; 2; 2; 1; 1; 2; 2; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_5 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 2; 2; 1; 2; 3|];
  [|3; 0; 1; 2; 2; 2; 1; 1; 2; 3|];
  [|3; 0; 2; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 2; 2; 2; 2; 2; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_5_1 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 2; 2; 1; 2; 3|];
  [|3; 1; 1; 2; 2; 2; 1; 1; 2; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 2; 3|];
  [|3; 2; 2; 1; 2; 2; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board3_5_2 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 2; 2; 1; 1; 2; 3|];
  [|3; 1; 1; 2; 2; 2; 2; 1; 2; 3|];
  [|3; 1; 2; 2; 2; 2; 1; 1; 2; 3|];
  [|3; 2; 2; 2; 2; 1; 1; 2; 2; 3|];
  [|3; 2; 2; 1; 2; 2; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board4 = 
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]; 
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|]; 
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 2; 1; 1; 2; 1; 1; 1; 3|]; 
  [|3; 2; 2; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 0; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
let final_board4 =
  (*   A  B  C  D  E  F  G  H  *)
[|[|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|];
  [|3; 1; 2; 2; 2; 2; 2; 2; 2; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 0; 0; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 2; 1; 1; 2; 1; 1; 1; 3|];
  [|3; 1; 1; 2; 2; 1; 1; 1; 1; 3|];
  [|3; 1; 1; 1; 2; 1; 1; 2; 1; 3|];
  [|3; 0; 1; 1; 1; 1; 2; 1; 2; 3|];
  [|3; 1; 1; 1; 1; 1; 1; 1; 1; 3|];
  [|3; 3; 3; 3; 3; 3; 3; 3; 3; 3|]|]
