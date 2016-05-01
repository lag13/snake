package termbox

import (
	"github.com/lag13/snake/Go/snakegame"
	"github.com/nsf/termbox-go"
)

type gameRenderer struct{}

func (r gameRenderer) Render(gs snakegame.GameState) {
	termbox.Clear(termbox.ColorDefault, termbox.ColorDefault)
	drawBorder(gs.Height, gs.Width)
	drawSnake(gs.Snake)
	drawFood(gs.Food)
	termbox.Flush()
}

func setCell(x int, y int, ch rune) {
	termbox.SetCell(x, y, ch, termbox.ColorDefault, termbox.ColorDefault)
}

func drawBorder(height int, width int) {
	for y := 0; y < height; y++ {
		setCell(width, y, '|')
	}
	for x := 0; x < width; x++ {
		setCell(x, height, '-')
	}
	setCell(width, height, '+')
}

func drawSnake(snake []snakegame.Pt) {
	for _, pt := range snake {
		setCell(pt.X, pt.Y, '#')
	}
}

func drawFood(food snakegame.Pt) {
	setCell(food.X, food.Y, '@')
}

func InitTermbox(out chan<- rune) (gameRenderer, func(), error) {
	err := termbox.Init()
	if err != nil {
		return gameRenderer{}, func() {}, err
	}
	go pollForInput(out)
	return gameRenderer{}, termbox.Close, nil
}

func pollForInput(out chan<- rune) {
	for {
		event := termbox.PollEvent()
		out <- event.Ch
	}
}
