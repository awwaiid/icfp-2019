
open Printf

let test_count = ref 0

let ok msg result =
  test_count := !test_count + 1;
  match result with
  | true  -> printf "ok %i - %s\n" !test_count msg
  | false -> printf "not ok %i - %s\n" !test_count msg

let is msg a b =
  if a == b then
    ok msg true
  else
    ok msg false

let plan n =
  printf "1..%i\n" n

let done_testing () =
  printf "1..%i\n" !test_count

