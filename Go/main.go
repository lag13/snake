package main

import (
	"log"

	"github.com/lag13/snake/Go/ncurses"
	"github.com/lag13/snake/Go/snakegame"
	"github.com/lag13/snake/Go/termbox"
	"github.com/lag13/snake/ncurses2"
)

// TODO: Display a score

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
	// displayPlatform := "ncurses"
	displayPlatform := "termbox"
	// displayPlatform := "ncurses2"
	renderer, cleanup, inputStream, err := initDisplayPlatform(displayPlatform)
	if err != nil {
		log.Println(err)
		return
	}
	defer cleanup()
	gameState := snakegame.InitSnakeGame(height, width, inputStream)
	snakegame.GameLoop(renderer, gameState)
}

// TODO: See if we can get testing around the rendering stuff. Like we can
// make an abstraction for the rendering code and then we can test that? Or is
// that just helpful/not worth it/doesn't make any sense to do.

// TODO: Move this into a new package and put all specific platforms below
// that package.
type cleanupDisplayPlatform func()

func initDisplayPlatform(displayPlatform string) (renderer snakegame.Renderer, cleanup cleanupDisplayPlatform, inputStream chan rune, err error) {
	inputStream = make(chan rune)
	switch {
	case displayPlatform == "ncurses":
		renderer, cleanup, err = ncurses.InitNcurses(inputStream)
	case displayPlatform == "ncurses2":
		renderer, cleanup, err = ncurses2.InitNcurses(inputStream)
	default:
		renderer, cleanup, err = termbox.InitTermbox(inputStream)
	}
	return renderer, cleanup, inputStream, err
}
