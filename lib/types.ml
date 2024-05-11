type symbol = | Number of int
              | String of string
              | Identifier of string
              | Function of (string list) * (symbol list)
              | List of symbol list;;


let rec print_symbol (x: symbol) : string =
  let open List in
  match x with
  | Number(n) -> (string_of_int n) ^ " "
  | Identifier(s) -> s ^ " "
  | String(s) -> s ^ " "
  | Function(l, r) -> "(" ^ (fold_left (^) "" l) ^ ")"
                      ^ " => " ^ ((map print_symbol r)
                                  |> fold_left (^) "") ^ " "
  | List(l) -> "[ " ^ ((map print_symbol l)
                       |> fold_left (^) "") ^ "]"

