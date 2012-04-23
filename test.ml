open TetravexModel

(* this program expects the 3 arguments: m n c *)
let argv = Sys.argv;;
let m = int_of_string argv.(1);;
let n = int_of_string argv.(2);;
let c = int_of_string argv.(3);;

let _ = begin
  (* [Puzzle.generate] generates a well-formed puzzle (has a solution) *)
  (* if you want a true random generation, use [Puzzle.dumb_generate] *)
  let puzzle = Puzzle.generate m n c in
  Printf.printf "Puzzle:\n";
  Puzzle.print puzzle;
  Puzzle.solve puzzle;
  Printf.printf "\nSolution:\n";
  Puzzle.print puzzle;
end;;
