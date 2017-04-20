package terminal

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

func getChar() rune {
	event := termbox.PollEvent()
	return event.Ch
}

func Init() (func(), func(int, int, rune), func(), func() rune, func(), error) {
	if err := termbox.Init(); err != nil {
		return clear, setCell, renderGame, getChar, termbox.Close, err
	}
	return clear, setCell, renderGame, getChar, termbox.Close, nil
}
