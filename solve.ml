let puzzle = Puzzle.read_in () in begin
  Puzzle.solve puzzle;
  Puzzle.print puzzle;
end;;

