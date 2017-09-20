package main

import (
	"fmt"
	"math/rand"
	"time"

	"github.com/lag13/snake/go/snake"
	termbox "github.com/nsf/termbox-go"
)

// A list of other people's implementations of snake:
// https://golanglibs.com/top?q=snake.

// TODO: How would we write acceptance tests for this?

func main() {
	width := 15
	height := 15
	sleep := 100 * time.Millisecond
	seed := rand.NewSource(time.Now().UnixNano())
	playerActions := make(chan snake.PlayerAction, 2)
	if err := termbox.Init(); err != nil {
		panic(err)
	}
	defer termbox.Close()
	keys := []rune{'w', 's', 'a', 'd', 'p', 'q', 'n'}
	controls := map[rune]snake.PlayerAction{
		keys[0]: snake.Up,
		keys[1]: snake.Down,
		keys[2]: snake.Left,
		keys[3]: snake.Right,
		keys[4]: snake.Pause,
		keys[5]: snake.Quit,
		keys[6]: snake.NewGame,
	}
	go getInput(playerActions, controls)
	snake.Play(width, height, seed, renderer{controls: controls, keys: keys}, playerActions, sleep)
}

type renderer struct {
	// We use this to always iterate over the map in the same
	// order.
	keys     []rune
	controls map[rune]snake.PlayerAction
}

// TODO: In my older (much more complicated) implementation the
// specific rendering engine used was abstracted behind various
// functions which was kind of cool. Mayber I should do that again
// when I make a graphical implementation of this? I also, just for
// fun, want to use ncurses with this program which also means I might
// want to have this abstraction.
func (r renderer) Render(g snake.GameState) {
	drawChar := func(x int, y int, ch rune) {
		termbox.SetCell(x, y, ch, termbox.ColorDefault, termbox.ColorDefault)
	}
	drawStr := func(x int, y int, s string) {
		for i, ch := range s {
			drawChar(x+i, y, ch)
		}
	}
	drawCharOnGrid := func(x int, y int, ch rune) {
		drawChar(2*x, y, ch)
	}
	drawStrOnGrid := func(x int, y int, s string) {
		for i, ch := range s {
			drawCharOnGrid(x+i, y, ch)
		}
	}
	termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
	for y := 0; y < g.Height; y++ {
		for x := 0; x < g.Width; x++ {
			drawCharOnGrid(x, y, '.')
		}
	}
	for y, key := range r.keys {
		actionDesc := ""
		switch r.controls[key] {
		case snake.Up:
			actionDesc = "move up"
		case snake.Down:
			actionDesc = "move down"
		case snake.Left:
			actionDesc = "move left"
		case snake.Right:
			actionDesc = "move right"
		case snake.Pause:
			actionDesc = "pause"
		case snake.Quit:
			actionDesc = "quit"
		case snake.NewGame:
			actionDesc = "start a new game"
		}
		drawStr(2*g.Width, y, fmt.Sprintf("%c - %s", key, actionDesc))
	}
	for _, snakeSegment := range g.Snake[:len(g.Snake)-1] {
		drawCharOnGrid(snakeSegment.X, snakeSegment.Y, '#')
	}
	drawCharOnGrid(g.Snake[len(g.Snake)-1].X, g.Snake[len(g.Snake)-1].Y, '@')
	drawCharOnGrid(g.Food.X, g.Food.Y, '*')
	if g.Paused {
		msg := "[Paused]"
		drawStrOnGrid(g.Width/2-len(msg)/2, g.Height/2, msg)
	}
	drawStr(0, g.Height, fmt.Sprintf("Score: %d", g.Score))
	if g.GameIsLost {
		drawStr(0, g.Height+1, "You lost. Then again one usually \"loses\" at snake")
	} else if g.GameIsWon {
		drawStr(0, g.Height+1, "Holy shit you won!!")
	}
	termbox.Flush()
}

func getInput(playerActions chan<- snake.PlayerAction, controls map[rune]snake.PlayerAction) {
	for {
		event := termbox.PollEvent()
		action, ok := controls[event.Ch]
		if ok {
			playerActions <- action
		}
	}
}
