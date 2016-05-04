package main

import (
	"log"

	"github.com/lag13/snake/Go/render/displayPlatform"
	"github.com/lag13/snake/Go/snakegame"
)

// TODO: Be able to pause and quit the game

// TODO: Make an AI

// TODO: Be able to pass in command line flags to change the size of the board

// TODO: Maybe draw the board in the middle of the terminal instead of the
// upper left hand corner.

// TODO: Make it possible to change the sleep duration to speedup/slowdown the
// game

func main() {
	height := 15
	width := 15
	// whichDisplayPlatform := "ncurses"
	// whichDisplayPlatform := "ncurses2"
	whichDisplayPlatform := "termbox"
	renderer, cleanup, inputStream, err := displayPlatform.InitDisplayPlatform(whichDisplayPlatform)
	if err != nil {
		log.Println(err)
		return
	}
	defer cleanup()
	gameState := snakegame.InitSnakeGame(height, width, inputStream)
	snakegame.GameLoop(renderer, gameState)
}
