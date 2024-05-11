open Yare.Lexer

let print_token (a: token) =
  match a with
  | Token(t) -> "<" ^ t ^ "> "
  | _ -> "<>";;

let print_tokens r =
  match r with
  | Ok(l) -> List.map (fun (a, _) -> print_token a) l
  | _ -> ["Error!\n"];;

let () = "let miao = lalala '[\"uwu\"]"
         |> lexer
         |> print_tokens
         |> List.iter print_endline;;
