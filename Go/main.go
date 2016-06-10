package main

import (
	"flag"
	"fmt"
	"log"

	"github.com/lag13/snake/Go/controller"
	"github.com/lag13/snake/Go/model"
	"github.com/lag13/snake/Go/terminal"
	"github.com/lag13/snake/Go/view"
)

// TODO: Make an AI

// TODO: Maybe draw the board in the middle of the terminal instead of the
// upper left hand corner. Perhaps make this configurable. Maybe also make the
// game itself bounce around the screen. That could be fun.

func main() {
	height := flag.Int("h", 20, "height of game")
	width := flag.Int("w", 20, "width of game")
	sleep := flag.Int("s", 100, "how long the game sleeps in between flags (ms)")
	flag.Parse()
	inputStream := make(chan rune)
	clearScreen, drawCell, refreshScreen, cleanup, err := terminal.Init(inputStream)
	if err != nil {
		log.Println(err)
		return
	}
	renderer := view.New(clearScreen, drawCell, refreshScreen)
	inputHandler := controller.New(inputStream)
	gameState := model.InitSnakeGame(*height, *width, *sleep, inputHandler)
	score := model.GameLoop(renderer, gameState)
	cleanup()
	fmt.Printf("Score was %d\n", score)
}
