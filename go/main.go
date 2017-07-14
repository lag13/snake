package main

import (
	"flag"
	"fmt"
	"log"
	"time"

	"github.com/lag13/snake/Go/ai"
	"github.com/lag13/snake/Go/controller"
	"github.com/lag13/snake/Go/model"
	"github.com/lag13/snake/Go/snake"
	"github.com/lag13/snake/Go/terminal"
	"github.com/lag13/snake/Go/view"
)

// A list of other people's implementations of snake:
// https://golanglibs.com/top?q=snake.

// TODO: Make the ai still able to accept user input for pausing and quiting. I
// think this means we'll be getting a second little game loop.

func main() {
	height := flag.Int("h", 20, "height of game")
	width := flag.Int("w", 20, "width of game")
	sleep := flag.Int("s", 100, "how long the game sleeps in between frames (ms)")
	playWithAI := flag.Bool("ai", false, "have an ai play the game")
	flag.Parse()
	clearScreen, drawCell, refreshScreen, getChar, cleanup, err := terminal.Init()
	if err != nil {
		log.Println(err)
		return
	}
	gameState := model.New(*height, *width)
	renderer := view.New(clearScreen, drawCell, refreshScreen)
	var inputGetter controller.InputGetter
	if !*playWithAI {
		inputStream := make(chan rune)
		go pollForInput(inputStream, getChar)
		inputGetter = controller.New(inputStream)
	} else {
		inputGetter = ai.New(gameState)
	}
	score := snake.Play(gameState, renderer, inputGetter, time.Duration(*sleep)*time.Millisecond)
	// TODO: Flush input buffer before returning to the terminal so random
	// characters that were pressed don't show up.
	cleanup()
	fmt.Printf("Score was %d\n", score)
}

func pollForInput(out chan<- rune, getChar func() rune) {
	for {
		out <- getChar()
	}
}
