package snake

import (
	"reflect"
	"testing"

	"github.com/lag13/snake/Go/controller"
	"github.com/lag13/snake/Go/model"
)

type mockGameInput struct {
	inputIndex int
	inputs     []int
}

func (mgi *mockGameInput) GetInput() int {
	if mgi.inputIndex == len(mgi.inputs) {
		return mgi.inputs[len(mgi.inputs)-1]
	}
	mgi.inputIndex++
	return mgi.inputs[mgi.inputIndex-1]
}

type mockRenderer struct {
	gameStates []model.GameState
}

func (mr *mockRenderer) Render(gs model.GameState) {
	mr.gameStates = append(mr.gameStates, gs)
}

type mockFoodGenerator struct {
	foodIndex     int
	foodPositions []model.Pt
}

func (mfg *mockFoodGenerator) GenerateFood(height int, width int, snake []model.Pt) model.Pt {
	if len(mfg.foodPositions) == 0 {
		return model.Pt{-1, -1}
	}
	if mfg.foodIndex == len(mfg.foodPositions) {
		return model.Pt{-1, -1}
	}
	mfg.foodIndex++
	return mfg.foodPositions[mfg.foodIndex-1]
}

// TestPlay is basically an E2E test (I think) of this game with controlled
// inputs/generated food positions etc...
func TestPlay(t *testing.T) {
	const (
		height = 4
		width  = 4
	)
	tests := []struct {
		renderer        *mockRenderer
		gameInput       *mockGameInput
		foodGenerator   *mockFoodGenerator
		wantSnakeStates [][]model.Pt
	}{
		{
			&mockRenderer{},
			&mockGameInput{0, []int{controller.Down, controller.Right, controller.Right, controller.Up, controller.Up, controller.Left, controller.Down, controller.Down, controller.Up, controller.Right, controller.Up, controller.Down, controller.Up, controller.Left}},
			&mockFoodGenerator{0, []model.Pt{{1, 2}, {2, 2}, {2, 3}, {3, 3}, {1, 0}}},
			[][]model.Pt{ // Test snake runs around for a while then dies running into a wall
				[]model.Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{3, 2}, {2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}},
				[]model.Pt{{3, 1}, {3, 2}, {2, 2}, {1, 2}, {1, 1}, {1, 0}},
				[]model.Pt{{3, 0}, {3, 1}, {3, 2}, {2, 2}, {1, 2}, {1, 1}},
				[]model.Pt{{2, 0}, {3, 0}, {3, 1}, {3, 2}, {2, 2}, {1, 2}},
				[]model.Pt{{2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {2, 2}},
				[]model.Pt{{2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]model.Pt{{2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]model.Pt{{3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]model.Pt{{3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}},
				[]model.Pt{{3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}},
				[]model.Pt{{3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}},
				[]model.Pt{{2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}},
				[]model.Pt{{1, 0}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}},
				[]model.Pt{{0, 0}, {1, 0}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}},
			},
		},
		{ // Test snake dies by running into itself
			&mockRenderer{},
			&mockGameInput{0, []int{controller.Down, controller.Right, controller.Up, controller.Left}},
			&mockFoodGenerator{0, []model.Pt{{1, 2}}},
			[][]model.Pt{
				[]model.Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}},
				[]model.Pt{{2, 1}, {2, 2}, {1, 2}, {1, 1}, {1, 0}},
			},
		},
		{ // Test that quitting exits the game
			&mockRenderer{},
			&mockGameInput{0, []int{controller.Down, controller.Quit, controller.Up, controller.Left}},
			&mockFoodGenerator{0, []model.Pt{{1, 2}}},
			[][]model.Pt{
				[]model.Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]model.Pt{{1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
			},
		},
	}
	for _, test := range tests {
		gameState := model.New(height, width)
		gameState.FoodGen = test.foodGenerator
		gameState.Food = test.foodGenerator.GenerateFood(gameState.Height, gameState.Width, gameState.Snake)
		Play(gameState, test.renderer, test.gameInput, 0)
		if len(test.wantSnakeStates) != len(test.renderer.gameStates) {
			t.Errorf("got %d iterations of the game loop, want %d", len(test.renderer.gameStates), len(test.wantSnakeStates))
		} else {
			for i, snakeState := range test.wantSnakeStates {
				if !reflect.DeepEqual(snakeState, test.renderer.gameStates[i].Snake) {
					t.Errorf("after %d game loop iterations got snake %v, want %v", i, test.renderer.gameStates[i].Snake, snakeState)
				}
			}
		}
	}
}
