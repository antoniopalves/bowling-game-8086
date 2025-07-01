# 🎳 Bowling Game in Assembly (EMU8086)

A 2-player bowling game written entirely in **x86 Assembly**, built for the **EMU8086** emulator.  
Developed as a final project for the *Microprocessadores* course at FCT/UNL.

##  Overview

Players take turns launching a ball to knock down pins, gaining points for each hit.  
Includes a graphical interface, score logging, a Top 5 leaderboard, and menu navigation with the mouse.

##  Project Structure

 Bowling Game

├── Trabalho_Final.asm # Main source code (Assembly)

├── TOP5.BIN # Top 5 score data (binary format)

├── Results.txt # Game logs (names, scores, timestamps)

├── Relatorio_58339_59205_55379.pdf # Project report (Portuguese)


##  Features

-  2-player turn-based gameplay
- Control via arrow keys and Enter
-  Mouse-controlled main menu:
  - Play
  - Top 5
  - Credits
  - Exit
-  Score logging with timestamps in `Results.txt`
-  Top 5 leaderboard (saved in `TOP5.BIN`)
-  Credits screen and developer info

##  Running the Game

> ⚙ Requires **EMU8086** installed.

1. Open `Trabalho_Final.asm` in EMU8086.
2. Compile the code.
3. Run in the emulator (Full or Step-by-Step).
4. Use **mouse** to navigate menu.
5. Use **arrow keys** to aim the ball and **Enter** to launch it.

##  Known Issues (as per report)

- Menu graphics have a slight visual bug on "Jogar".
- `Results.txt` logs all scores in a single line instead of one per row.
- Rounds only start incrementing correctly from the second round onward.
- Top 5 logic partially works (checks/creates file, but doesn't always update correctly).

##  Authors

- António Alves - Nº 58339  
- Cátia Carraça - Nº 59205  

##  Report

The full project report (in Portuguese) is included:  
📄 `Relatorio_58339_59205_55379.pdf`



