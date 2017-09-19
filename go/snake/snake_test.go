package snake_test

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing"

	"github.com/lag13/snake/go/snake"
)

type mockRenderer struct {
	gameStates []snake.GameState
}

func (m *mockRenderer) Render(g snake.GameState) {
	copySnake := make([]snake.Point, len(g.Snake))
	copy(copySnake, g.Snake)
	copyG := g
	copyG.Snake = copySnake
	m.gameStates = append(m.gameStates, copyG)
}

// TestPlay tests that the game of snake goes through the expected
// states given certain inputs. It is sort of like an acceptance test
// (although it is not very end to end because we're not interacting
// with the program from the outside world). TODO: These tests aren't
// really practical because its not easy to control the channel of
// inputs to effect the game and if that channel doesn't end in a Quit
// then the test will loop forever. I wonder if that "quitting" and
// "restarting" logic should go into a separate package? Or just a
// better way to structure things? These tests are so difficult that
// I'm stopping writing them.
func TestPlay(t *testing.T) {
	tests := []struct {
		testScenario         string
		width                int
		height               int
		rngSrc               rand.Source
		renderer             *mockRenderer
		playerActions        chan snake.PlayerAction
		initialPlayerActions []snake.PlayerAction
		wantGameStates       []snake.GameState
	}{
		{
			testScenario:         "The game terminates immediately (in a victory!) since the board is the size of the snake and the player chooses to quit",
			width:                2,
			height:               2,
			rngSrc:               rand.NewSource(0),
			renderer:             &mockRenderer{[]snake.GameState{}},
			playerActions:        make(chan snake.PlayerAction, 16),
			initialPlayerActions: []snake.PlayerAction{},
			wantGameStates: []snake.GameState{
				{
					Width:  2,
					Height: 2,
					Snake: []snake.Point{
						{X: 0, Y: 1},
						{X: 0, Y: 0},
						{X: 1, Y: 0},
						{X: 1, Y: 1},
					},
					Food:       snake.Point{X: -1, Y: -1},
					Paused:     false,
					Score:      0,
					GameIsWon:  true,
					GameIsLost: false,
				},
			},
		},
		{
			testScenario:         "The snake eats food and gets longer",
			width:                3,
			height:               3,
			rngSrc:               rand.NewSource(0),
			renderer:             &mockRenderer{[]snake.GameState{}},
			playerActions:        make(chan snake.PlayerAction, 16),
			initialPlayerActions: []snake.PlayerAction{snake.Right, snake.Down},
			wantGameStates: []snake.GameState{
				{
					Width:  3,
					Height: 3,
					Snake: []snake.Point{
						{X: 0, Y: 1},
						{X: 0, Y: 0},
						{X: 1, Y: 0},
						{X: 1, Y: 1},
					},
					Food:       snake.Point{X: 2, Y: 2},
					Paused:     false,
					Score:      0,
					GameIsWon:  false,
					GameIsLost: false,
				},
				{
					Width:  3,
					Height: 3,
					Snake: []snake.Point{
						{X: 0, Y: 0},
						{X: 1, Y: 0},
						{X: 1, Y: 1},
						{X: 2, Y: 1},
					},
					Food:       snake.Point{X: 2, Y: 2},
					Paused:     false,
					Score:      0,
					GameIsWon:  false,
					GameIsLost: false,
				},
				{
					Width:  3,
					Height: 3,
					Snake: []snake.Point{
						{X: 0, Y: 0},
						{X: 1, Y: 0},
						{X: 1, Y: 1},
						{X: 2, Y: 1},
						{X: 2, Y: 2},
					},
					Food:       snake.Point{X: 0, Y: 2},
					Paused:     false,
					Score:      0,
					GameIsWon:  false,
					GameIsLost: false,
				},
			},
		},
	}
	for i, test := range tests {
		errorMsg := func(str string, args ...interface{}) {
			t.Helper()
			t.Errorf("Running test %d, where %s:\n"+str, append([]interface{}{i, test.testScenario}, args...)...)
		}
		for _, playerAction := range test.initialPlayerActions {
			test.playerActions <- playerAction
		}
		test.playerActions <- snake.Quit
		snake.Play(test.width, test.height, test.rngSrc, test.renderer, test.playerActions, 0)
		if errMsg := gotExpectedGameStates(test.renderer.gameStates, test.wantGameStates); errMsg != "" {
			errorMsg(errMsg)
		}
	}
}

func gotExpectedGameStates(gotGameStates []snake.GameState, wantGameStates []snake.GameState) string {
	if got, want := len(gotGameStates), len(wantGameStates); got != want {
		return fmt.Sprintf("%d game state(s) were rendered, should have rendered %d game state(s)", got, want)
	}
	for i := range gotGameStates {
		if got, want := gotGameStates[i], wantGameStates[i]; !reflect.DeepEqual(got, want) {
			return fmt.Sprintf("on the %d game state:\ngot    %+v\nwanted %+v", i, got, want)
		}
	}
	return ""
}
