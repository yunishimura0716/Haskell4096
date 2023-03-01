# Haskell4096

# Setup Instruction
1. Make sure your machine can run `stack` command
    - If not, you can refer to [this link](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. set up the project by the following command. Download the compiler (if necessary): stack setup

    ```stack setup```
3. Build the project:

    ```stack build```
4. Run executable file

    ```stack exec haskell4098-exe```

# How to play
1. You choose which type of board. We have
    - `4 * 4`, `5 * 5` or `6 * 6`
2. Once a game begin, you can play 2048 with direction keys

3. If you end the game, just type `Ctrl-C` on the console

# ToDo
- [x] be able to allow the users to choose the size of board (4 * 4, 5 * 5, 6 * 6, n * n)
- scoring logic (if possible)
- [x] write a test
- [ ] correct animation logic to work for combined movement and merge
- [ ] add
- [ ] create a docs
