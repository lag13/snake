package snakegame

import (
	"math/rand"
	"reflect"
	"testing"
	"time"
)

func TestGameContinues(t *testing.T) {
	tests := []struct {
		boardHeight int
		boardWidth  int
		snake       []Pt
		want        bool
	}{
		{1, 1, []Pt{{1, 1}}, false},
		{10, 10, []Pt{{-1, 0}, {0, 0}}, false},
		{10, 10, []Pt{{11, 5}, {10, 5}}, false},
		{0, 0, []Pt{{0, 0}, {1, 0}}, false},
		{-10, -10, []Pt{{0, 0}, {1, 0}}, false},
		{10, 10, []Pt{{0, 5}, {0, 4}, {0, 3}}, true},
		{10, 10, []Pt{{0, 5}, {0, 4}, {0, 3}, {1, 3}, {1, 4}, {1, 5}, {0, 5}}, false},
		{10, 10, []Pt{{0, 5}, {0, 5}}, false},
	}
	for _, test := range tests {
		got := gameContinues(test.boardHeight, test.boardWidth, test.snake)
		if got != test.want {
			t.Errorf("gameContinues(%v, %v, %v) = %v", test.boardHeight, test.boardWidth, test.snake, got)
		}
	}
}

func TestGenerateFoodDoesNotHitSnake(t *testing.T) {
	// Tests if it is not possible to generate food when the snake occupies the
	// whole board or that if there is only one place to generate food it gets
	// generated there
	foodGenerator := buildDefaultFoodGenerator()
	tests := []struct {
		height int
		width  int
		snake  []Pt
		want   Pt
	}{
		{-1, -3, []Pt{{0, 0}}, Pt{-1, -1}},
		{0, 0, []Pt{{0, 0}}, Pt{-1, -1}},
		{1, 1, []Pt{{0, 0}}, Pt{-1, -1}},
		{1, 1, []Pt{{-1, 0}}, Pt{0, 0}},
		{2, 2, []Pt{{0, 0}, {0, 1}, {1, 0}}, Pt{1, 1}},
	}
	for _, test := range tests {
		got := foodGenerator.generateFood(test.height, test.width, test.snake)
		if got != test.want {
			t.Errorf("generateFood(%v, %v, %v) = %v, wanted %v", test.height, test.width, test.snake, got, test.want)
		}
	}
	// Randomly generating the food position to give some assurance that the
	// generated food position does not conflict with the snake's position
	height := 10
	width := 10
	snake := generateSnakeCoveringMostOfBoard(height, width)
	seed := time.Now().Unix()
	rng := rand.New(rand.NewSource(seed))
	foodGenerator = &defaultFoodGenerator{rng}
	for i := 0; i < 100; i++ {
		food := foodGenerator.generateFood(height, width, snake)
		if positionExists(food, snake) {
			t.Errorf("generated a food position %v that hits the snake %v, seed for rng was %v and this was the %d random number generated", food, snake, seed, i)
		}
	}
}

func generateSnakeCoveringMostOfBoard(height int, width int) []Pt {
	snake := []Pt{}
	for y := 0; y < height-2; y++ {
		for x := 0; x < width-2; x++ {
			snake = append(snake, Pt{x, y})
		}
	}
	return snake
}

func TestInputReturnsAppropriateDirection(t *testing.T) {
	tests := []struct {
		input  rune
		curDir Pt
		want   Pt
	}{
		{'h', Pt{0, 0}, Pt{-1, 0}},
		{'j', Pt{0, 0}, Pt{0, 1}},
		{'k', Pt{0, 0}, Pt{0, -1}},
		{'l', Pt{0, 0}, Pt{1, 0}},
		{'q', Pt{1, 0}, Pt{1, 0}},
		{'h', Pt{1, 0}, Pt{1, 0}},
		{'j', Pt{0, -1}, Pt{0, -1}},
		{'k', Pt{0, 1}, Pt{0, 1}},
		{'l', Pt{-1, 0}, Pt{-1, 0}},
	}
	for _, test := range tests {
		got := getDirectionFromInput(test.input, test.curDir)
		if got != test.want {
			t.Errorf("getDirectionFromInput('%c', %v) = %v, wanted %v", test.input, test.curDir, got, test.want)
		}
	}
}

type mockGameInput struct {
	inputIndex int
	inputs     []rune
}

func (mgi *mockGameInput) GetInput() rune {
	if mgi.inputIndex == len(mgi.inputs) {
		return mgi.inputs[len(mgi.inputs)-1]
	}
	mgi.inputIndex++
	return mgi.inputs[mgi.inputIndex-1]
}

type mockSleeper struct{}

func (ms *mockSleeper) Sleep() {
}

type mockRenderer struct {
	gameStates []GameState
}

func (mr *mockRenderer) Render(gs GameState) {
	mr.gameStates = append(mr.gameStates, gs)
}

type mockFoodGenerator struct {
	foodIndex     int
	foodPositions []Pt
}

func (mfg *mockFoodGenerator) generateFood(height int, width int, snake []Pt) Pt {
	if len(mfg.foodPositions) == 0 {
		return Pt{-1, -1}
	}
	if mfg.foodIndex == len(mfg.foodPositions) {
		return Pt{-1, -1}
	}
	mfg.foodIndex++
	return mfg.foodPositions[mfg.foodIndex-1]
}

// TODO: Does this test in some way make other tests obsolete because I'm
// testing this game from start to finish?
func TestGameBehavesAsExpectedGivenInput(t *testing.T) {
	const (
		height = 4
		width  = 4
	)
	ms := &mockSleeper{}
	tests := []struct {
		renderer        *mockRenderer
		gameInput       *mockGameInput
		foodGenerator   *mockFoodGenerator
		wantSnakeStates [][]Pt
	}{
		{
			&mockRenderer{},
			&mockGameInput{0, []rune{'j', 'l', 'l', 'k', 'k', 'h', 'j', 'j', 'k', 'l', 'k', 'j', 'k', 'h'}},
			&mockFoodGenerator{0, []Pt{{1, 2}, {2, 2}, {2, 3}, {3, 3}, {1, 0}}},
			[][]Pt{ // Test snake runs around for a while then dies running into a wall
				[]Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]Pt{{1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]Pt{{2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]Pt{{3, 2}, {2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}},
				[]Pt{{3, 1}, {3, 2}, {2, 2}, {1, 2}, {1, 1}, {1, 0}},
				[]Pt{{3, 0}, {3, 1}, {3, 2}, {2, 2}, {1, 2}, {1, 1}},
				[]Pt{{2, 0}, {3, 0}, {3, 1}, {3, 2}, {2, 2}, {1, 2}},
				[]Pt{{2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {2, 2}},
				[]Pt{{2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]Pt{{2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]Pt{{3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}, {3, 2}},
				[]Pt{{3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}, {3, 1}},
				[]Pt{{3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}, {3, 0}},
				[]Pt{{3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}, {2, 0}},
				[]Pt{{2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}},
				[]Pt{{1, 0}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}, {2, 1}},
				[]Pt{{0, 0}, {1, 0}, {2, 0}, {3, 0}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {2, 2}},
			},
		},
		{ // Test snake dies by running into itself
			&mockRenderer{},
			&mockGameInput{0, []rune{'j', 'l', 'k', 'h'}},
			&mockFoodGenerator{0, []Pt{{1, 2}}},
			[][]Pt{
				[]Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]Pt{{1, 2}, {1, 1}, {1, 0}, {0, 0}, {0, 1}},
				[]Pt{{2, 2}, {1, 2}, {1, 1}, {1, 0}, {0, 0}},
				[]Pt{{2, 1}, {2, 2}, {1, 2}, {1, 1}, {1, 0}},
			},
		},
	}
	for _, test := range tests {
		gameState := InitSnakeGame(height, width, nil)
		gameState.foodGenerator = test.foodGenerator
		gameState.Food = test.foodGenerator.generateFood(gameState.Height, gameState.Width, gameState.Snake)
		gameState.inputGetter = test.gameInput
		gameState.Sleeper = ms
		GameLoop(test.renderer, gameState)
		// TODO: Go preferrs tests to say got this want this instead of the
		// reverse like I'm doing here. Fix this.
		if len(test.wantSnakeStates) != len(test.renderer.gameStates) {
			t.Errorf("want %d iterations of the game loop, got %d", len(test.wantSnakeStates), len(test.renderer.gameStates))
		} else {
			for i, snakeState := range test.wantSnakeStates {
				if !reflect.DeepEqual(snakeState, test.renderer.gameStates[i].Snake) {
					t.Errorf("after %d game loop iterations want snake to be %v, got %v", i, snakeState, test.renderer.gameStates[i].Snake)
				}
			}
		}
	}
}
