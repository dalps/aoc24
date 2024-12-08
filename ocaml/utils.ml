module L = CCList
module O = CCOption
module P = CCParse
module R = CCResult
module A = CCArray
module F = CCFun
module Pr = Printf

let pr = Pr.printf
let spr = Pr.sprintf


let read_file (filename : string) : string =
  let fd = open_in filename in
  let s = really_input_string fd (in_channel_length fd) in
  close_in fd; s

let is_transitive (l : (int * int) list) =
  let open L in
  fold_left ( && ) true @@
  let* x, y = l and* y', z = l in
  return ((not (Int.equal y y')) || List.mem (x, z) l)
