package ncurses

import (
	"github.com/lag13/snake/Go/render"
	"github.com/lag13/snake/Go/snakegame"
	"github.com/mpatraw/gocurse/curses"
)

func InitNcurses(out chan<- rune) (r snakegame.Renderer, cleanup func(), err error) {
	screen, err := curses.Initscr()
	if err != nil {
		return r, cleanup, err
	}
	curses.Curs_set(0)
	// This always seems to return an error even though it works
	curses.Noecho()
	go pollForInput(screen, out)
	var setCell = func(x int, y int, ch rune) { screen.Addch(x, y, ch, 0) }
	var refresh = func() { screen.Refresh() }
	return render.NewSimpleRenderer(screen.Clear, setCell, refresh), func() { curses.Endwin() }, nil
}

func pollForInput(screen *curses.Window, out chan<- rune) {
	for {
		out <- rune(screen.Getch())
	}
}
