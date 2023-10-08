open OUnit2
module Tokenizer = Toylang.Tokenizer

let test_scan _ =
  assert_equal
    (Toylang.scan_all "foobar_12 0-\n5 * 500 + 12345")
    [
      Tokenizer.Identifier "foobar_12";
      Tokenizer.Integer 0;
      Tokenizer.BinaryOperator '-';
      Tokenizer.Integer 5;
      Tokenizer.BinaryOperator '*';
      Tokenizer.Integer 500;
      Tokenizer.BinaryOperator '+';
      Tokenizer.Integer 12345;
    ]

let () = run_test_tt_main ("test_tokenizer" >::: [ "test_scan" >:: test_scan ])
