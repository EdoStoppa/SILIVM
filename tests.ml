(* First execute prj.ml, only then execute this!*)

let state0 = update empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))

let state1 = update (update state0 "x" (Val (IntVal 1)))
  "y" (Val (IntVal 2))
  
let config1 = (Return (Add (Var "x", Var "y")), [(state0, "x")], state1)

let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_s "y";; (* should return None *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_s "y";; (* should return None *)