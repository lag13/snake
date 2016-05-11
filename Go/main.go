package main

import (
	"flag"
	"fmt"
	"log"

	"github.com/lag13/snake/Go/render/displayPlatform"
	"github.com/lag13/snake/Go/snakegame"
)

// TODO: Make an AI

// TODO: Maybe draw the board in the middle of the terminal instead of the
// upper left hand corner. Perhaps make this configurable. Maybe also make the
// game itself bounce around the screen. That could be fun.

// TODO: Make it possible to change the sleep duration to speedup/slowdown the
// game

func main() {
	height := flag.Int("h", 20, "height of game")
	width := flag.Int("w", 20, "width of game")
	whichDisplayPlatform := flag.String("d", "termbox", "the terminal package used to display the game")
	// sleep := flag.Int("s", 100, "how long the game sleeps in between flags (ms)")
	flag.Parse()
	renderer, cleanup, inputStream, err := displayPlatform.InitDisplayPlatform(*whichDisplayPlatform)
	if err != nil {
		log.Println(err)
		return
	}
	gameState := snakegame.InitSnakeGame(*height, *width, inputStream)
	score := snakegame.GameLoop(renderer, gameState)
	cleanup()
	fmt.Printf("Score was %d\n", score)
}
