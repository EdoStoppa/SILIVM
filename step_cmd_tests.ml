(* Tests for function step_cmd *)


(* Assign tests *)
let config0 = (Assign ("x", Add (Num 1, Num 2)),
               [(empty_state, "__exit")],
               empty_state)
let (asg_0_c, asg_0_k, asg_0_s) = run_config config0;;
lookup_state asg_0_s "x";; (* Should return Some Val IntVal 3 *)

let config0 = (Assign ("x", Add (Vec (1::[]), Num 2)),
               [(empty_state, "__exit")],
               empty_state)

let (asg_1_c, asg_1_k, asg_1_s) = run_config config0;;
lookup_state asg_1_s "x";; (* Should return Some Val VecVal [3] *)


(* Seq tests *)
let seq_0 = step_cmd (Seq(Skip, Assign ("x", Add(Num 1, Num 2)))) [(empty_state, "__exit")] empty_state
(* Should return Some (Assign(...), ..., ...) *)
let seq_1 = step_cmd (Seq(Assign ("x", Add(Num 1, Num 2)), Seq (Skip, Skip))) [(empty_state, "__exit")] empty_state
(* Should return Some (Seq(...), ..., ...) *)


(* IfC tests *)
let if_0 = step_cmd (IfC(Bool true, Assign ("x", Add(Num 1, Num 2)), Skip)) [(empty_state, "__exit")] empty_state
(* Should return Some (Assign(...), ..., ...) *)
let if_1 = step_cmd (IfC(Bool false, Assign ("x", Add(Num 1, Num 2)), Skip)) [(empty_state, "__exit")] empty_state
(* Should return Some (Skip, ..., ...) *)


(* While tests *)
let whl_0 = step_cmd (While(Bool true, Assign ("x", Add(Num 1, Num 2)))) [(empty_state, "__exit")] empty_state
(* Should return Some (IfC(...), ..., ...) *)


(* Tests Return and Call *)
let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))
let state1 = update_state (update_state state0 "x" (Val (IntVal 1)))
    "y" (Val (IntVal 2))
    
let config1 = (Return (Add (Var "x", Var "y")), [(state0, "x")], state1)
let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup_state res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "y";; (* should return None *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup_state res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "y";; (* should return None *)


(* Mixed tests *)
let state2 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))
let state3 = update_state state2 "g" (Fun (["a"; "x"], Seq (Call("y", "f", [Var "x"; Num 4]), (IfC (Eq(Var "y", Num 6), Return (Var "y"), Return (Var "a"))))))

let prog2 = Call("z", "g", [Num 1; Num 3])
let prog3 = Call("z", "g", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_prog prog2 state3;;
lookup_state res_s "a";; (* should return None *)
lookup_state res_s "x";; (* should return None *)
lookup_state res_s "y";; (* should return None *)
lookup_state res_s "z";; (* should return Some (Val (IntVal 1)) *)

let (res_c, res_k, res_s) = run_prog prog3 state3;;
lookup_state res_s "a";; (* should return None *)
lookup_state res_s "x";; (* should return None *)
lookup_state res_s "y";; (* should return None *)
lookup_state res_s "z";; (* should return Some (Val (IntVal 6)) *)