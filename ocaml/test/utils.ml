open AOC24.Utils
open Coord

let%test "test_coord_x" =
  let p = make 0 1 in
  x p = 0
let%test "test_coord_y" =
  let p = make 0 1 in
  y p = 1
let%expect_test "test_y_again" =
  make 0 1 |> y |> print_int;
  [%expect {| 1 |}]
