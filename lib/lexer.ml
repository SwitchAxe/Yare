type token = | Token of string
             | Error of string
let (++) (x: token * string) (y: token * string) =
  match (x, y) with
  | ((Token(a), _), (Token(b), s')) -> (Token(a^b), s')
  | _ -> x



(* if the first char satisfies p,
   it returns c. returns an Error otherwise. *)

let accept (s: string) (p: char -> bool) : token * string =
  match s |> String.to_seq |> List.of_seq with
  | ch :: rest when p ch -> (Token(String.make 1 ch),
                             rest |> List.to_seq |> String.of_seq)
  | _ -> (Error((String.get s 0) |> String.make 1), s);;

(* a few common predicates using the function above *)

let accept_specific (s: string) (c: char) = accept
                                              s
                                              (fun (x) -> x = c);;

let accept_digit (s: string) = accept
                                 s
                                 (function
                                  | '0'..'9' -> true
                                  | _ -> false);;

let accept_letter (s: string) = accept
                                  s
                                  (function
                                   | 'a'..'z' | 'A'..'Z' -> true
                                   | _ -> false);;

(* a few more complex procedures built
   on top of the ones above. *)


let rec accept_number (s: string) : token * string =
  match (accept_digit s) with
  | (Token(_), r) as x -> x ++ (accept_number r)
  | (Error(_), _) as y -> y;; 

let accept_identifier(s: string) : token * string =
  let special = ['('; ')';
                 '['; ']';
                 '{'; '}';
                 '|'; ',';
                 ' '; '\t']
  in
  let rec aux s =
    match (accept s (fun (c) -> not (List.mem c special))) with
    | (Token(_), s') as tp -> tp ++ (aux s')
    | (Error(_), _) as tp -> tp
  in aux s;;

let accept_string (s: string) : token * string =
  let tail s = String.sub s 1 ((String.length s) - 1)
  in
  let rec aux s =
    match (accept_specific s '"') with
    | (Token(_), _) as tp -> tp
    | (Error(t), s') -> (Token(t), s') ++ (aux (tail s'))
  in match (accept_specific s '"') with
     | (Token(_), s') as tp -> tp ++ (aux s')
     | (Error(_), _) as tp -> tp;;



(* A lexer procedure.
   The lexer applies all the above functions
   to the input until it's empty. *)

let lexer (s: string) : ((token * int) list, int) result =
  let rec aux (s: string) (ln: int) : (token * int) list =
    match s with
    | "" -> []
    | _ -> 
       let (tk, r) =
         match s.[0] with
         | '0'..'9' -> accept_number s
         | '"' -> accept_string s
         | '['|']'|'('|')'|'{'|'}'|'|'|','|'\n'|' '|'\t' as c ->
            accept_specific s c
         | _ -> accept_identifier s
       in match tk with
          | Token(" ") -> aux r ln
          | Token("\t") -> aux r ln
          | Token("\n") -> aux r (ln + 1)
          | Token(_) as t -> (t, ln) :: (aux r ln)
          | Error(_) as e -> [(e, ln)]
  in
  let l = aux s 0
  in
  match List.find_opt
          (fun (a, _) -> match a with
                         | Error(_) -> true
                         | _ -> false) l with
  | Some(Error(_), ln) -> Error(ln)
  | _ -> Ok(l);;
