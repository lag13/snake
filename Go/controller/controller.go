// Package control takes human input and maps it into an internal
// representation which will be used by the model to appropriately update the
// game state.
package controller

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
	'h': Left,
	'j': Down,
	'k': Up,
	'l': Right,
	'p': Pause,
	'q': Quit,
}

type handler struct {
	inputStream <-chan rune
}

func New(inputStream <-chan rune) *handler {
	return &handler{inputStream}
}

func (h *handler) GetInput() int {
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
