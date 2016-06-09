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

// TODO: What is a good way to pass "special" keys like the arrow keys through
// this channel in such a way that if we switched out termbox and useed
// something else then that "something else" can use it as well and we'll only
// have to make changes here and everything will still work. I would imagine
// that we'd have to define a constant for every key on the keyboard and map
// the termbox key to one of those constants and send that down the channel.
// The controller package will use those constants and so it won't have to
// worry about whether the keys are being served by termbox or ncurses or what
// have you.
func pollForInput(out chan<- rune) {
	for {
		event := termbox.PollEvent()
		out <- event.Ch
	}
}
