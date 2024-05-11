
module Lexer = struct
  let tester
        (f: string -> ('a, 'b) result)
        (a: string) (b: ('a, 'b) result)
        (label: string) : unit =
    if (f a) = b then print_endline ("Test " ^ label ^ ": PASSED")
    else print_endline ("Test " ^ label ^ ": FAILED");;
end
