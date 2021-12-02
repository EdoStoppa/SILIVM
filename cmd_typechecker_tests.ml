let gamma0 : context = fun y ->
  if y = "x" || y = "y" then Some IntTy else None

let gamma6 : context = update_context (update_context empty_context "f" (FunTy (IntTy, [IntTy]))) "x" IntTy

let cmd0 : cmd =
  Assign("x", Add(Num 1, Num 2))

let cmd1 : cmd =
Seq (Assign("x", Add(Num 1, Num 2)),
     Assign("y", Add(Num 4, Num 5)))

let cmd2 : cmd =
Seq (Assign("x", Add(Num 1, Num 2)),
  Seq (Assign("x", Add(Num 1, Num 2)),
      Assign("y", Add(Num 4, Num 5))))   

let cmd3 : cmd =
Seq (Assign("x", Add(Num 1, Num 2)),Skip)

let cmd4 : cmd =
IfC (Bool false, Assign("x", Add(Num 4, Num 4)), Skip)
let cmd5 : cmd =
While(Bool false, Assign("x", Add(Num 4, Num 4)))

let cmd6 : cmd =
Call("x", "f", [Num 2])


  
let test0 = typecheck_cmd gamma0 cmd0 (* should return true *)

let test1 = typecheck_cmd gamma0 cmd1 (* should return true *)

let test2 = typecheck_cmd gamma0 cmd2 (* should return true *)

let test3 = typecheck_cmd gamma0 cmd3 (* should return true *)

let test4 = typecheck_cmd gamma0 cmd4 (* should return true *)

let test5 = typecheck_cmd gamma0 cmd5 (* should return true *)
let test6 = typecheck_cmd gamma6 cmd6 (* should return true *)