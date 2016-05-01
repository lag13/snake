package ncurses2

import (
	"github.com/lag13/snake/Go/snakegame"
	"github.com/rthornton128/goncurses"
)

type gameRenderer struct {
	screen *goncurses.Window
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
		r.screen.MoveAddChar(y, width, '|')
	}
	for x := 0; x < width; x++ {
		r.screen.MoveAddChar(height, x, '-')
	}
	r.screen.MoveAddChar(height, width, '+')
}

func (r gameRenderer) drawSnake(snake []snakegame.Pt) {
	for _, pt := range snake {
		r.screen.MoveAddChar(pt.Y, pt.X, '#')
	}
}

func (r gameRenderer) drawFood(food snakegame.Pt) {
	r.screen.MoveAddChar(food.Y, food.X, '@')
}

func InitNcurses(out chan<- rune) (gameRenderer, func(), error) {
	screen, err := goncurses.Init()
	if err != nil {
		return gameRenderer{}, func() {}, err
	}
	goncurses.Echo(false)
	goncurses.Cursor(0)
	go pollForInput(screen, out)
	return gameRenderer{screen}, goncurses.End, nil
}

// func InitNcurses(out chan<- rune) (gameRenderer, func(), error) {
// 	screen, err := curses.Initscr()
// 	if err != nil {
// 		return gameRenderer{}, func() {}, err
// 	}
// 	curses.Curs_set(0)
// 	// This always seems to return an error even though it works
// 	curses.Noecho()
// 	go pollForInput(screen, out)
// 	return gameRenderer{screen}, func() { curses.Endwin() }, nil
// }

func pollForInput(screen *goncurses.Window, out chan<- rune) {
	for {
		out <- rune(screen.GetChar())
	}
}
