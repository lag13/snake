package termbox

import (
	"github.com/lag13/snake/Go/render"
	"github.com/lag13/snake/Go/snakegame"
	"github.com/nsf/termbox-go"
)

func clear() {
	termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
}

func setCell(x int, y int, ch rune) {
	termbox.SetCell(x, y, ch, termbox.ColorDefault, termbox.ColorDefault)
}

func renderGame() {
	termbox.Flush()
}

func InitTermbox(out chan<- rune) (r snakegame.Renderer, cleanup func(), err error) {
	if err = termbox.Init(); err != nil {
		return r, cleanup, err
	}
	go pollForInput(out)
	return render.NewSimpleRenderer(clear, setCell, renderGame), termbox.Close, nil
}

func pollForInput(out chan<- rune) {
	for {
		event := termbox.PollEvent()
		out <- event.Ch
	}
}
