package render

import (
	"fmt"

	"github.com/lag13/snake/Go/snakegame"
)

// TODO: See if we can get testing around the rendering stuff. Like we can
// make an abstraction for the rendering code and then we can test that? Or is
// that just helpful/not worth it/doesn't make any sense to do.

type clearScreen func()

type drawCell func(x int, y int, ch rune)

type refreshScreen func()

type simpleRenderer struct {
	clear   clearScreen
	dc      drawCell
	refresh refreshScreen
}

func (sr simpleRenderer) Render(gs snakegame.GameState) {
	sr.clear()
	drawBorder(sr.dc, gs.Height, gs.Width)
	drawSnake(sr.dc, gs.Snake)
	drawFood(sr.dc, gs.Food)
	drawScore(sr.dc, gs.Height+1, len(gs.Snake))
	sr.refresh()
}

func NewSimpleRenderer(clear clearScreen, dc drawCell, refresh refreshScreen) simpleRenderer {
	return simpleRenderer{clear, dc, refresh}
}

func drawBorder(dc drawCell, height int, width int) {
	for y := 0; y < height; y++ {
		dc(2*width, y, '|')
	}
	for x := 0; x < 2*width; x += 2 {
		dc(x, height, '-')
	}
	dc(2*width, height, '+')
}

func drawSnake(dc drawCell, snake []snakegame.Pt) {
	for _, pt := range snake {
		dc(2*pt.X, pt.Y, '#')
	}
}

func drawFood(dc drawCell, food snakegame.Pt) {
	dc(2*food.X, food.Y, '@')
}

func drawScore(dc drawCell, height int, score int) {
	drawStringf(dc, height, 0, "Score: %d", score)
}

func drawStringf(dc drawCell, y int, x int, s string, args ...interface{}) {
	str := fmt.Sprintf(s, args...)
	for i, c := range str {
		dc(x+i, y, c)
	}
}
