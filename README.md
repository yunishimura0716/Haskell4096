# Haskell4096

# ToDo
- fix a bug on board logic if anything
- implement a function to check the given direction is allowable
  - shift board with given direction
  - check if it changes the original
  - I think this is easy to Eq class for Board
- implement a function to findout the current game state can continue or end
  - we are going to call the above function with every direction
- implement Bounce Effect (add new property to Grid, e.g. bounce)
- be able to allow the users to choose the size of board (4 * 4, 5 * 5, 6 * 6)
- write a test
- create a docs