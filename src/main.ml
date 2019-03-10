open Lexer
open Parser
open Ast

exception CKError of string
let ck_error msg = raise (CKError msg)

let is_v v = match v with
  | Con _ | Abs _ -> true
  | _ -> false

let rec sub x v p =
  let _rec e = sub x v e in
  match p with
  | Var y -> if x = p then v else p
  | Con b -> p
  | Abs (y, m) -> if x = Var y then p else Abs (y, _rec m)
  | App (m, n) -> App (_rec m, _rec n)
  | Prm (o, es) -> Prm (o, List.map _rec es)

let eval t = match t with
  | ST (App (m, n), k) -> ST (m, Ar (n, k))
  | ST (v, Fn(Abs(x, m), k)) when is_v v -> ST (sub (Var x) v m, k)
  | ST (v, Ar(n, k)) when is_v v -> ST (n, Fn (v, k))
  | ST (Prm(o, m::n), k) -> ST (m, Pr(o, [], n, k))
  | ST (v, Pr (o, u, m::n, k)) when is_v v -> ST (m, Pr (o, u@[v], n, k))
  | ST (b, Pr (o, bs, [], k)) -> begin match o with
    | "+" -> ST (Con (List.fold_left (fun acc (Con i) -> acc + i) 0 (b::bs)), k)
    | _ -> ck_error "Unknown primitive operation"
    end
  | _ -> ck_error "No matching state"

let rec eval_program s =
  match s with
  | ST (v, Mt) when is_v v -> s
  | _ -> eval_program (eval s)

let run s = eval_program @@ ST (s, Mt)

let rec repl () =
  print_string "> ";
  let str = read_line () in
  let buffer = Lexing.from_string str in
  let ast = main token buffer in
  print_endline @@ str_st @@ run ast;
  repl ()

let _ = repl ()
