package ai

import (
	"github.com/lag13/snake/Go/controller"
	"github.com/lag13/snake/Go/model"
)

type aiInputGetter struct {
	gs *model.GameState
	// Used in the naiveZigZag() approach.
	travellingLeft bool
}

func New(gs *model.GameState) *aiInputGetter {
	return &aiInputGetter{gs, false}
}

func (ai *aiInputGetter) GetInput() int {
	return ai.naiveZigZag()
}

var (
	left  = model.Pt{-1, 0}
	down  = model.Pt{0, 1}
	up    = model.Pt{0, -1}
	right = model.Pt{1, 0}
)

// Originally I thought this strategy of zig zagging would be a naive way to
// win turns out I was totally wrong!
func (ai *aiInputGetter) naiveZigZag() int {
	if ai.gs.CurDirection == down {
		if ai.gs.Snake[0].Y < ai.gs.Height-2 {
			return controller.Down
		}
		if ai.gs.Snake[0].X == 0 && ai.gs.Snake[0].Y == ai.gs.Height-2 {
			ai.travellingLeft = false
			return controller.Right
		}
		if ai.gs.Snake[0].X == ai.gs.Width-1 && ai.gs.Snake[0].Y == ai.gs.Height-2 {
			ai.travellingLeft = true
			return controller.Left
		}
		if ai.travellingLeft {
			return controller.Left
		}
		return controller.Right
	}
	if ai.gs.CurDirection == up {
		if ai.gs.Snake[0].Y > 0 {
			return controller.Up
		}
		if ai.gs.Snake[0].X == 0 && ai.gs.Snake[0].Y == 0 {
			ai.travellingLeft = false
			return controller.Right
		}
		if ai.gs.Snake[0].X == ai.gs.Width-1 && ai.gs.Snake[0].Y == 0 {
			ai.travellingLeft = true
			return controller.Left
		}
		if ai.travellingLeft {
			return controller.Left
		}
		return controller.Right
	}
	if ai.gs.CurDirection == right {
		if ai.gs.Snake[0].X == ai.gs.Width-1 && ai.gs.Snake[0].Y == 0 {
			return controller.Down
		}
		if ai.gs.Snake[0].X == ai.gs.Width-1 && ai.gs.Snake[0].Y == ai.gs.Height-2 {
			return controller.Up
		}
		if ai.gs.Snake[0].Y == 0 {
			return controller.Down
		}
		return controller.Up
	}
	if ai.gs.CurDirection == left {
		if ai.gs.Snake[0].X == 0 && ai.gs.Snake[0].Y == 0 {
			return controller.Down
		}
		if ai.gs.Snake[0].X == 0 && ai.gs.Snake[0].Y == ai.gs.Height-2 {
			return controller.Up
		}
		if ai.gs.Snake[0].Y == 0 {
			return controller.Down
		}
		return controller.Up
	}
	return controller.None
}
