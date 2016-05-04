package displayPlatform

import (
	"github.com/lag13/snake/Go/render/displayPlatform/ncurses"
	"github.com/lag13/snake/Go/render/displayPlatform/ncurses2"
	"github.com/lag13/snake/Go/render/displayPlatform/termbox"
	"github.com/lag13/snake/Go/snakegame"
)

type cleanupDisplayPlatform func()

func InitDisplayPlatform(displayPlatform string) (renderer snakegame.Renderer, cleanup cleanupDisplayPlatform, inputStream chan rune, err error) {
	inputStream = make(chan rune)
	switch {
	case displayPlatform == "ncurses":
		renderer, cleanup, err = ncurses.InitNcurses(inputStream)
	case displayPlatform == "ncurses2":
		renderer, cleanup, err = ncurses2.InitNcurses(inputStream)
	default:
		renderer, cleanup, err = termbox.InitTermbox(inputStream)
	}
	return renderer, cleanup, inputStream, err
}
