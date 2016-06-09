package termbox

import "github.com/nsf/termbox-go"

func clear() {
	termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
}

func setCell(x int, y int, ch rune) {
	termbox.SetCell(x, y, ch, termbox.ColorDefault, termbox.ColorDefault)
}

func renderGame() {
	termbox.Flush()
}

func Init(out chan<- rune) (func(), func(int, int, rune), func(), func(), error) {
	if err := termbox.Init(); err != nil {
		return clear, setCell, renderGame, termbox.Close, err
	}
	go pollForInput(out)
	return clear, setCell, renderGame, termbox.Close, nil
}

func pollForInput(out chan<- rune) {
	for {
		event := termbox.PollEvent()
		out <- event.Ch
	}
}
