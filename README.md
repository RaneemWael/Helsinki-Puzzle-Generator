# Helsinki-Puzzle-Generator
A prolog code that generates a Helsinki puzzle of size N.

## Description:
- Given a square grid of size N, where the horizontal rows are numbered 1 to N from top to bottom and the vertical columns are numbered 1 to N from left to right.
- You must place a number in each cell of the N by N grid such that :-
  - Each row is unique.
  - Each row is exactly equal to one of the columns, however, it must not be the column with the same index as the row.
  - If X is the largest number you place in the grid, then you must also place 1,2,...,X-1, where the condition X <= N is satisfied.

## How to run:
- Download the .pl file to your computer.
- Import into prolog.
- Run the comman with a grid of a size of your choice.
