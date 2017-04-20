package model

import (
	"math/rand"
	"reflect"
	"testing"
	"time"

	"github.com/lag13/snake/Go/controller"
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
		gs := New(test.boardHeight, test.boardWidth)
		gs.Snake = test.snake
		got := gs.GameContinues()
		if got != test.want {
			t.Errorf("GameContinues() with height = %d, width = %d, and snake = %v was %v", test.boardHeight, test.boardWidth, test.snake, got)
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
		got := foodGenerator.GenerateFood(test.height, test.width, test.snake)
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
		food := foodGenerator.GenerateFood(height, width, snake)
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
		input  int
		curDir Pt
		want   Pt
	}{
		{controller.Left, Pt{0, 0}, Pt{-1, 0}},
		{controller.Down, Pt{0, 0}, Pt{0, 1}},
		{controller.Up, Pt{0, 0}, Pt{0, -1}},
		{controller.Right, Pt{0, 0}, Pt{1, 0}},
		{controller.Quit, Pt{1, 0}, Pt{1, 0}},
		{controller.Left, Pt{1, 0}, Pt{1, 0}},
		{controller.Down, Pt{0, -1}, Pt{0, -1}},
		{controller.Up, Pt{0, 1}, Pt{0, 1}},
		{controller.Right, Pt{-1, 0}, Pt{-1, 0}},
	}
	for _, test := range tests {
		got := getDirectionFromInput(test.input, test.curDir)
		if got != test.want {
			t.Errorf("getDirectionFromInput('%c', %v) = %v, wanted %v", test.input, test.curDir, got, test.want)
		}
	}
}

func TestUpdateGameState(t *testing.T) {
	const (
		height = 10
		width
	)
	tests := []struct {
		input         int
		direction     Pt
		foodPos       Pt
		snake         []Pt
		score         int
		paused        bool
		wantDirection Pt
		wantSnake     []Pt
		wantScore     int
		wantPaused    bool
	}{
		// Trying to move in a direction opposite the current direction keeps the snake moving in the current direction.
		{controller.Up, down, Pt{3, 2}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, false, down, []Pt{{4, 5}, {4, 4}, {3, 4}, {2, 4}}, 0, false},
		// Moving orthogonal to direction of travel works.
		{controller.Left, down, Pt{3, 2}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, false, left, []Pt{{3, 4}, {4, 4}, {3, 4}, {2, 4}}, 0, false},
		// Eating the food grows the snake and increases the score.
		{controller.Right, down, Pt{5, 4}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, false, right, []Pt{{5, 4}, {4, 4}, {3, 4}, {2, 4}, {1, 4}}, 1, false},
		// If the game is paused and they try to move then the game state does not change.
		{controller.Right, down, Pt{5, 4}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, true, down, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, true},
		// If the game is paused and they unpause it then the game changes.
		{controller.Pause, down, Pt{5, 4}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, true, down, []Pt{{4, 5}, {4, 4}, {3, 4}, {2, 4}}, 0, false},
		// If the game is not paused and they pause it then the game state does not change.
		{controller.Pause, down, Pt{5, 4}, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, false, down, []Pt{{4, 4}, {3, 4}, {2, 4}, {1, 4}}, 0, true},
	}
	for i, test := range tests {
		errorMsg := func(str string, args ...interface{}) {
			t.Errorf(str+" Running test %d", append(args, i)...)
		}
		gs := New(height, width)
		gs.CurDirection = test.direction
		gs.Food = test.foodPos
		gs.Snake = test.snake
		gs.Score = test.score
		gs.Paused = test.paused
		gs.UpdateGameState(test.input)
		if gs.CurDirection != test.wantDirection {
			errorMsg("got direction %v, wanted %v.", gs.CurDirection, test.wantDirection)
		}
		if !reflect.DeepEqual(gs.Snake, test.wantSnake) {
			errorMsg("got snake %v, wanted %v.", gs.Snake, test.wantSnake)
		}
		if gs.Score != test.wantScore {
			errorMsg("got score %v, wanted %v.", gs.Score, test.wantScore)
		}
		if gs.Paused != test.wantPaused {
			errorMsg("got paused %v.", gs.Paused)
		}
	}
}
