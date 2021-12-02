let gamma0 : context = fun y ->
  if y = "x" || y = "y" then Some IntTy else None
let gamma00 : context = fun y ->
  if y = "x" || y = "y" then Some BoolTy else None

let gamma000 : context = fun y ->
  if y = "x" || y = "y" then Some VecTy else None

let gamma0000 : context = fun y ->
  if y = "__ret" then Some IntTy else None
      
let gamma7 : context = update_context empty_context "__ret" IntTy

let cmd0 : cmd =
  Assign("x", Add(Num 1, Num 2))

let cmd01 : cmd =
  Assign("x", And(Bool true, Bool false))

let cmd02 : cmd =
  Assign("x", Mul(Num 7, Num 6))
let cmd03 : cmd =
  Assign("x", Mul(Vec [4;5;6], Vec[1;2;3]))

let cmd1 : cmd =
  Seq (Assign("x", Add(Num 1, Num 2)),
  Assign("y", Sub(Num 8, Num 5)))

let cmd11 : cmd =
  Seq (Assign("x", Mul(Vec [3;5;7], Vec [7;9;11])),
  Assign("y", Add(Vec [1;2;3], Vec [2;3;4])))
  

let cmd12 : cmd =
  Seq (Assign("x", Add(Num 1, Num 2)),
  Seq (Assign("x", Add(Num 1, Num 2)),
  Assign("y", Add(Num 4, Num 5))))   


let cmd2 : cmd =
  Return (Num 6)

let cmd3 : cmd =
  Skip
let cmd4 : cmd =
  IfC (Eq(Num 6, Num 8), Assign("x", Add(Num 4, Num 4)), Skip)

let cmd41 : cmd =
  IfC (Eq(Vec [1;2;3], Vec[1;2;3]), Assign("x", Add(Vec [4;2], Vec [1;3])), Skip)


let cmd5 : cmd =
  While(Bool false, Assign("x", Add(Num 4, Num 4)))

let cmd51 : cmd =
  While(Bool true, Assign("x", Mul(Vec [4;5;5], Vec[1;0;4])))

let cmd52 : cmd =
  While(Eq(Var "x", Num 7), Assign("x", Add(Var "x", Num 1)))

let cmd6 : cmd =
  Call("x", "f", [Num 2])


let gamma6 : context = update_context (update_context empty_context "f" (FunTy (IntTy, [IntTy]))) "x" IntTy


let gamma61 : context = update_context (update_context empty_context "f" (FunTy (IntTy, [IntTy;BoolTy]))) "x" IntTy


let gamma62 : context = update_context (update_context empty_context "f" (FunTy (IntTy, [IntTy;BoolTy;VecTy]))) "x" IntTy

let cmd61 : cmd =
  Call("x", "f", [Num 2;Bool true])
  

let cmd62 : cmd =
  Call("x", "f", [Num 2;Bool true;Vec [2;3;4;4;5;5]])

let test0 = typecheck_cmd gamma0 cmd0 (* should return true *)
let test01 = typecheck_cmd gamma00 cmd01 (* should return true *)
let test02 = typecheck_cmd gamma0 cmd02 (* should return true *)
let test03 = typecheck_cmd gamma000 cmd03 (* should return true *)

let test1 = typecheck_cmd gamma0 cmd1 (* should return true *)
let test11 = typecheck_cmd gamma000 cmd11 (* should return true *)

let test12 = typecheck_cmd gamma0 cmd12 (* should return true *)
let test2 = typecheck_cmd gamma7 cmd2 (* should return true *)

let test3 = typecheck_cmd gamma00 cmd3 (* should return true *)

let test4 = typecheck_cmd gamma0 cmd4 (* should return true *)
let test41 = typecheck_cmd gamma000 cmd41 (* should return true *)

let test5 = typecheck_cmd gamma0 cmd5 (* should return true *)

let test51 = typecheck_cmd gamma000 cmd51 (* should return true *)
let test52 = typecheck_cmd gamma0 cmd52 (* should return true *)
let test6 = typecheck_cmd gamma6 cmd6 (* should return true *)

let test61 = typecheck_cmd gamma61 cmd61 (* should return true *)

let test62 = typecheck_cmd gamma62 cmd62 (* should return true *)