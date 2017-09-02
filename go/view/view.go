package view

import (
	"fmt"

	"github.com/lag13/snake/Go/model"
)

type clearScreen func()

type drawCell func(x int, y int, ch rune)

type refreshScreen func()

type Renderer interface {
	Render(gs model.GameState)
}

type simpleRenderer struct {
	clear   clearScreen
	dc      drawCell
	refresh refreshScreen
}

type cleanupDisplayPlatform func()

func New(c clearScreen, d drawCell, r refreshScreen) Renderer {
	return simpleRenderer{c, d, r}
}

func spaceOutX(x int) int {
	return 2 * x
}

func (r simpleRenderer) drawGameCell(x int, y int, ch rune) {
	r.dc(spaceOutX(x)+spaceOutX(1), y+1, ch)
}

func (r simpleRenderer) Render(gs model.GameState) {
	r.clear()
	r.drawBorder(gs.Height, gs.Width)
	r.drawSnake(gs.Snake)
	r.drawFood(gs.Food)
	r.drawScore(gs.Height+2, gs.Score)
	if gs.Paused {
		r.drawPause(gs.Height/2, gs.Width)
	}
	r.refresh()
}

func (r simpleRenderer) drawPause(y int, x int) {
	s := "[Paused]"
	r.drawString(y, x-len(s)/2, s)
}

func (r simpleRenderer) drawBorder(height int, width int) {
	r.drawLeftBorder(height)
	r.drawTopBorder(width)
	r.drawRightBorder(height, spaceOutX(width+1))
	r.drawBottomBorder(height+1, width)
	r.drawCorners(height, width)
}

func (r simpleRenderer) drawLeftBorder(height int) {
	for y := 1; y <= height; y++ {
		r.dc(0, y, '|')
	}
}

func (r simpleRenderer) drawTopBorder(width int) {
	for x := 1; x <= width; x++ {
		r.dc(spaceOutX(x), 0, '-')
	}
}

func (r simpleRenderer) drawRightBorder(height int, width int) {
	for y := 1; y <= height; y++ {
		r.dc(width, y, '|')
	}
}

func (r simpleRenderer) drawBottomBorder(height int, width int) {
	for x := 1; x <= width; x++ {
		r.dc(spaceOutX(x), height, '-')
	}
}

func (r simpleRenderer) drawCorners(height int, width int) {
	// upper left
	r.dc(0, 0, '+')
	// lower left
	r.dc(0, height+1, '+')
	w := spaceOutX(width + 1)
	// lower right
	r.dc(w, height+1, '+')
	// upper right
	r.dc(w, 0, '+')
}

func (r simpleRenderer) drawSnake(snake []model.Pt) {
	for _, pt := range snake {
		r.drawGameCell(pt.X, pt.Y, '#')
	}
}

func (r simpleRenderer) drawFood(food model.Pt) {
	r.drawGameCell(food.X, food.Y, '@')
}

func (r simpleRenderer) drawScore(height int, score int) {
	r.drawStringf(height, 0, "Score: %d", score)
}

func (r simpleRenderer) drawStringf(y int, x int, s string, args ...interface{}) {
	r.drawString(y, x, fmt.Sprintf(s, args...))
}

func (r simpleRenderer) drawString(y int, x int, s string) {
	for i, c := range s {
		r.dc(x+i, y, c)
	}
}
