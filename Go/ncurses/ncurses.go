package ncurses

import (
	"github.com/lag13/snake/Go/snakegame"
	"github.com/mpatraw/gocurse/curses"
)

type gameRenderer struct {
	screen *curses.Window
}

func (r gameRenderer) Render(gs snakegame.GameState) {
	r.screen.Clear()
	r.drawBorder(gs.Height, gs.Width)
	r.drawSnake(gs.Snake)
	r.drawFood(gs.Food)
	r.screen.Refresh()
}

func (r gameRenderer) drawBorder(height int, width int) {
	for y := 0; y < height; y++ {
		r.screen.Addch(width, y, '|', 0)
	}
	for x := 0; x < width; x++ {
		r.screen.Addch(x, height, '-', 0)
	}
	r.screen.Addch(width, height, '+', 0)
}

func (r gameRenderer) drawSnake(snake []snakegame.Pt) {
	for _, pt := range snake {
		r.screen.Addch(pt.X, pt.Y, '#', 0)
	}
}

func (r gameRenderer) drawFood(food snakegame.Pt) {
	r.screen.Addch(food.X, food.Y, '@', 0)
}

func InitNcurses(out chan<- rune) (gameRenderer, func(), error) {
	screen, err := curses.Initscr()
	if err != nil {
		return gameRenderer{}, func() {}, err
	}
	curses.Curs_set(0)
	// This always seems to return an error even though it works
	curses.Noecho()
	go pollForInput(screen, out)
	return gameRenderer{screen}, func() { curses.Endwin() }, nil
}

func pollForInput(screen *curses.Window, out chan<- rune) {
	for {
		out <- rune(screen.Getch())
	}
}

func Endwin() {
	curses.Endwin()
}
