package ncurses2

import (
	"github.com/lag13/snake/Go/render"
	"github.com/lag13/snake/Go/snakegame"
	"github.com/rthornton128/goncurses"
)

func InitNcurses(out chan<- rune) (r snakegame.Renderer, cleanup func(), err error) {
	screen, err := goncurses.Init()
	if err != nil {
		return r, cleanup, err
	}
	goncurses.Echo(false)
	goncurses.Cursor(0)
	go pollForInput(screen, out)
	clear := func() { screen.Clear() }
	setCell := func(x int, y int, ch rune) { screen.MoveAddChar(y, x, goncurses.Char(ch)) }
	return render.NewSimpleRenderer(clear, setCell, screen.Refresh), goncurses.End, nil
}

func pollForInput(screen *goncurses.Window, out chan<- rune) {
	for {
		out <- rune(screen.GetChar())
	}
}
