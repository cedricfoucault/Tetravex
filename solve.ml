open TetravexModel

let _ = begin
  (* read the puzzle from standard input *)
  let puzzle = Puzzle.read_in () in
    (* solve it *)
    Puzzle.solve puzzle;
    (* print it on standard output *)
    Puzzle.print puzzle;
end;;

