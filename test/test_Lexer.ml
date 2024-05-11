(* this file contains tests from the simple testing library
   in the Test module of the lib directory. *)


open Yare.Lexer
open Yare.Test.Lexer

let () =
  (tester
     lexer
     "\"aaabbbccc123123123\""
     (Ok([(Token("\"aaabbbccc123123123\""), 0)]))
     "simple strings");
  (tester
     lexer
     "123123123"
     (Ok([(Token("123123123"), 0)]))
     "simple numbers");
  (tester
     lexer
     "abcdefg"
     (Ok([(Token("abcdefg"), 0)]))
     "simple identifiers");
  (tester
     lexer
     "let a = b"
     (Ok([(Token("let"), 0);
          (Token("a"), 0);
          (Token("="), 0);
          (Token("b"), 0)]))
     "simple constant definition");
  (tester
     lexer
     "let a = (foo bar baz)"
     (Ok([(Token("let"), 0);
          (Token("a"), 0);
          (Token("="), 0);
          (Token("("), 0);
          (Token("foo"), 0);
          (Token("bar"), 0);
          (Token("baz"), 0);
          (Token(")"), 0)]))
     "complex constant definition");;
