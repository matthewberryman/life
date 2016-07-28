! Conway's life algorithm in ProcessBase
! featuring a (very) finite board
! (C) Matthew Berryman, 2002

! The board on which "life" occurs is 10x10
! but the internal representation is 11x11 to allow
! for "walls" of blank cells, to make coding the 
! algorithm easier

! includes
include safeOpLib ioLib syncLib threadLib

! init_cols_blank: return a blank spot in a column in a given row
let init_cols_blank <- fun(n: int) -> loc[bool]
begin
   let result <- loc(false)
   result
end

! init_cols: initialize a spot in a column in a given row to user input
let init_cols <- fun(n: int) -> loc[bool]
begin
   let result <- loc(false)
   if (n ~= 0) and (n ~= 11) do
      result := readBool(stdin)
   result
end

! init_rows_blank: returns a blank row
let init_rows_blank <- fun(n: int) -> *loc[bool]
   vector 0 to 11 using init_cols_blank

! returns a row with user entered cells
let init_rows <- fun(n: int) -> *loc[bool]
   if (n = 0) or (n = 11) then 
      vector 0 to 11 using init_cols_blank
   else 
      vector 0 to 11 using init_cols

! set up the board with the current state
let board <- vector 0 to 11 using init_rows

! set up a board for writing the next state to
let new_board <- vector 0 to 11 using init_rows_blank

! get_state: returns a (hopefully) unique integer given a unique board

let get_state <- fun() -> int
begin
   let result <- loc(0)
   for i <- 1 to 10 do
      for j <- 1 to 10 do
         ! not sure if the next line works 100% correctly
         ! seems ok though
         if 'board(i,j) do result := 'result + j*i + j*1000 + i*100000
   'result
end

! print_board: prints the currents state to screen
! * for a live cell, . for a barren one
! looks ok I think

let print_board <- fun()
begin
   for i <- 1 to 10 do {
      for j <- 1 to 10 do
         if 'board(i,j) then
            writeString(stdout, "* ", -1)
         else
            writeString(stdout, ". ", -1)
         writeString(stdout, "'n", -1)
   }
   writeString(stdout, "'n", -1)
end

! semaphore for doing barrier sync's
let barrier <- loc(newSemaphore(-10))
   
! change_state_thread: updates one column in new_board
! based on the state in board
! the whole (getCurrent() rem 10 + 1) thing really, really sucks
! because if the program runs for long enough this section of code
! will (possibly) break badly, but I haven't seen it do this yet

let change_state_thread <- fun()
begin
   for i <- 1 to 10 do {
      let score <- loc(0)
      if 'board(i-1, (getCurrent() rem 10 + 1)-1) do score := 'score + 1
      if 'board(i-1, (getCurrent() rem 10 + 1)) do score := 'score + 1
      if 'board(i-1, (getCurrent() rem 10 + 1)+1) do score := 'score + 1
      if 'board(i, (getCurrent() rem 10 + 1)-1) do score := 'score + 1
      if 'board(i, (getCurrent() rem 10 + 1)+1) do score := 'score + 1
      if 'board(i+1, (getCurrent() rem 10 + 1)-1) do score := 'score + 1
      if 'board(i+1, (getCurrent() rem 10 + 1)) do score := 'score + 1
      if 'board(i+1, (getCurrent() rem 10 + 1)+1) do score := 'score + 1

      if ('score < 2) do new_board(i, (getCurrent() rem 10 + 1)) := false
      !die horribly

      if ('score = 2) and 'board(i, (getCurrent() rem 10 + 1)) do
         new_board(i, (getCurrent() rem 10 + 1)) := true
         ! staying alive!         

      if ('score = 3) do new_board(i, (getCurrent() rem 10 + 1)) := true
      ! staying alive! / being born

      if ('score > 3) do new_board(i, (getCurrent() rem 10 + 1)) := false              
      ! die horribly
   }

   'barrier.signal() ! we're done!
end

! next bit is need only because we have to do _something_
! with the return value of start(change_state_thread),
! even though we don't do anything with it, since we only
! use the thread id in the thread function itself

let dummy_init <- fun(n: int) -> loc[int]; loc(n)
let test <- vector 1 to 10 using dummy_init

! Keep track of states, so we can break automagically
! if the pattern is a continuous repeater or 2-step oscillator.
! You could make the array size n+1 to detect n-step oscillators

let state <- vector @1 of [loc(get_state()), loc(0), loc(0)]
let looping <- loc(true)

while 'looping do
begin
   barrier := newSemaphore(-10)
   print_board()
   for i <- 1 to 10 do 
      test(i) := start(change_state_thread)
   'barrier.wait()
   for i <- 1 to 10 do
      for j <- 1 to 10 do
         board(i,j) := 'new_board(i,j)
   state(3) := 'state(2)
   state(2) := 'state(1)
   state(1) := get_state()
   if ('state(1) = 'state(2)) or ('state(1) = 'state(3)) do looping := false
end

print_board() ! shows up the repetition

