type m =
  | Var of string
  | Abs of string * m
  | App of m * m
  | Con of int
  | Prm of string * m list

let rec str_m m =
  match m with
  | Var id -> id
  | Abs (id, e) -> "λ" ^ id ^ "." ^ str_m e
  | App (fn, e) -> str_m fn ^ " " ^ str_m e
  | Con i -> string_of_int i
  | Prm (o, es) -> o ^ " " ^ String.concat " " (List.map str_m es)

type k =
  | Mt
  | Fn of m * k
  | Ar of m * k
  | Pr of string * m list * m list * k

let rec str_k k =
  match k with
  | Mt -> "⊥"
  | Fn (n, k) -> "fn (" ^ str_m n ^ " " ^ str_k k ^ ")"
  | Ar (v, k) -> "ar (" ^ str_m v ^ " " ^ str_k k ^ ")"
  | Pr (o, vs, ns, k) -> "op (" ^ o ^ ", [" ^ String.concat ", "
    (List.map str_m vs) ^ "], [" ^ String.concat ", " (List.map str_m ns) ^
    "], " ^ str_k k ^ ")"

type st =
  | ST of m * k

let str_st s =
  match s with
  | ST (m, k) -> str_m m ^ " , " ^ str_k k
