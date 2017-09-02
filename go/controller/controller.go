// Package controller takes human input and maps it into an internal
// representation which will be used by the model to appropriately update the
// game state. This allows us to easily change the controls for the game.
package controller

type InputGetter interface {
	GetInput() int
}

const (
	None = iota
	Up
	Down
	Left
	Right
	Pause
	Quit
)

var internalRepresentation = map[rune]int{
	'a': Left,
	's': Down,
	'w': Up,
	'd': Right,
	'p': Pause,
	'q': Quit,
}

type humanInputGetter struct {
	inputStream <-chan rune
}

func New(inputStream <-chan rune) *humanInputGetter {
	return &humanInputGetter{inputStream}
}

func (h *humanInputGetter) GetInput() int {
	select {
	case input := <-h.inputStream:
		ir, ok := internalRepresentation[input]
		if ok {
			return ir
		}
		return None
	default:
		return None
	}
}
