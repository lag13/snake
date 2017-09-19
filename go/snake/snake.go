// package snake defines the game loop for the game snake.
package snake

import (
	"math/rand"
	"time"
)

// PlayerAction is an action a player can take when playing snake.
type PlayerAction int

// These are the possible actions someone can take while playing.
const (
	NoAction PlayerAction = iota
	Up
	Down
	Left
	Right
	Pause
	Quit
	NewGame
)

// direction is a direction in 2D space.
type direction struct {
	X int
	Y int
}

// Point is a position in 2D space.
type Point struct {
	X int
	Y int
}

// GameState represents the state of the game of snake which gets
// drawn.
type GameState struct {
	Width      int
	Height     int
	Snake      []Point
	Food       Point
	Paused     bool
	Score      int
	GameIsWon  bool
	GameIsLost bool
}

// renderer renders the game of snake. It exists because I plan to
// have this same game logic used for a terminal or graphical display.
type renderer interface {
	Render(g GameState)
}

func initGameState(width int, height int, rng *rand.Rand) (GameState, direction) {
	gameState := GameState{
		Width:  width,
		Height: height,
		Snake: []Point{
			{X: 0, Y: 1},
			{X: 0, Y: 0},
			{X: 1, Y: 0},
			{X: 1, Y: 1},
		},
	}
	gameState.GameIsWon = gameIsWon(gameState)
	gameState.GameIsLost = gameIsLost(gameState)
	gameState.Food = genFood(gameState, rng)
	return gameState, direction{X: 0, Y: 1}
}

func Play(width int, height int, rngSrc rand.Source, r renderer, playerActions <-chan PlayerAction, sleepDur time.Duration) {
	rng := rand.New(rngSrc)
	gameState, curDir := initGameState(width, height, rng)
	for {
		r.Render(gameState)
		time.Sleep(sleepDur)
		input, newDir := getValidInput(playerActions, curDir)
		if input == Quit {
			break
		} else if input == Pause {
			gameState.Paused = !gameState.Paused
		} else if input == NewGame {
			gameState, curDir = initGameState(width, height, rng)
			continue
		} else {
			curDir = newDir
		}
		if !gameState.GameIsWon && !gameState.GameIsLost {
			gameState = updateGameState(gameState, curDir, rng)
		}
	}
}

func getValidInput(playerActions <-chan PlayerAction, curDir direction) (PlayerAction, direction) {
	for {
		select {
		case input := <-playerActions:
			if input == Quit || input == Pause || input == NewGame {
				return input, direction{}
			}
			newDir := playerActionToDirection(input)
			if directionsAreOrthogonal(curDir, newDir) {
				return input, newDir
			}
		default:
			return NoAction, curDir
		}
	}
}

func playerActionToDirection(p PlayerAction) direction {
	if p == Up {
		return direction{X: 0, Y: -1}
	}
	if p == Down {
		return direction{X: 0, Y: 1}
	}
	if p == Left {
		return direction{X: -1, Y: 0}
	}
	if p == Right {
		return direction{X: 1, Y: 0}
	}
	return direction{}
}

func directionsAreOrthogonal(d1 direction, d2 direction) bool {
	return d1.X*d2.X+d1.Y*d2.Y == 0
}

func updateGameState(gameState GameState, curDir direction, r *rand.Rand) GameState {
	if gameState.Paused {
		return gameState
	}
	snakeLen := len(gameState.Snake)
	newHead := addPointAndDir(gameState.Snake[snakeLen-1], curDir)
	if newHead == gameState.Food {
		gameState.Score++
		gameState.Snake = append(gameState.Snake, newHead)
		gameState.Food = genFood(gameState, r)
	} else {
		for i := 0; i < snakeLen-1; i++ {
			gameState.Snake[i] = gameState.Snake[i+1]
		}
		gameState.Snake[snakeLen-1] = newHead
	}
	gameState.GameIsLost = gameIsLost(gameState)
	gameState.GameIsWon = gameIsWon(gameState)
	return gameState
}

func addPointAndDir(p Point, d direction) Point {
	return Point{X: p.X + d.X, Y: p.Y + d.Y}
}

func genFood(gameState GameState, r *rand.Rand) Point {
	if gameState.GameIsWon || gameState.GameIsLost {
		return Point{X: -1, Y: -1}
	}
	whichPoint := r.Int31n(int32(gameState.Width*gameState.Height - len(gameState.Snake)))
	var i int32 = 0
	for y := 0; y < gameState.Height; y++ {
		for x := 0; x < gameState.Width; x++ {
			p := Point{X: x, Y: y}
			if containsPoint(gameState.Snake, p) {
				continue
			}
			if i == whichPoint {
				return p
			}
			i++
		}
	}
	return Point{-2, -2}
}

func containsPoint(ps []Point, q Point) bool {
	for _, p := range ps {
		if p == q {
			return true
		}
	}
	return false
}

func gameIsWon(gameState GameState) bool {
	if gameState.Width*gameState.Height == len(gameState.Snake) {
		return true
	}
	return false
}

func gameIsLost(gameState GameState) bool {
	snakeHead := gameState.Snake[len(gameState.Snake)-1]
	snakeTail := gameState.Snake[:len(gameState.Snake)-1]
	for _, snakeSegment := range snakeTail {
		if snakeHead == snakeSegment {
			return true
		}
	}
	if snakeHead.X < 0 || gameState.Width <= snakeHead.X {
		return true
	}
	if snakeHead.Y < 0 || gameState.Height <= snakeHead.Y {
		return true
	}
	return false
}
