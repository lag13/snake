// package snake defines the game loop for the game snake.
package snake

import (
	"math/rand"
	"time"
)

// TODO: Some logic in this package (like directions and points)
// should be moved into a separate package.

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
	curDir     direction
}

// renderer renders the game of snake. It exists because I plan to
// have this same game logic used for a terminal or graphical display.
type renderer interface {
	Render(g GameState)
}

func initGameState(width int, height int, rng *rand.Rand) GameState {
	gameState := GameState{
		Width:  width,
		Height: height,
		Snake: []Point{
			{X: 0, Y: 1},
			{X: 0, Y: 0},
			{X: 1, Y: 0},
			{X: 1, Y: 1},
		},
		curDir: playerActionToDirection(Down),
	}
	gameState.Food = genFood(gameState, rng)
	return gameState
}

// Play plays snake. TODO: I said it in my test for this function and
// I'll say it again here. I feel like there is a better way to
// organize this code. Maybe the Quit|NewGame aspects can be pulled
// out into their own package thereby reducing logic? That would free
// up this loop so it can only continue while the game is not lost.
func Play(width int, height int, rngSrc rand.Source, r renderer, playerActions <-chan PlayerAction, sleepDur time.Duration) {
	rng := rand.New(rngSrc)
	gameState := initGameState(width, height, rng)
	for {
		gameState.GameIsWon = gameIsWon(gameState)
		gameState.GameIsLost = gameIsLost(gameState)
		r.Render(gameState)
		time.Sleep(sleepDur)
		action, newDir := getValidInput(playerActions, gameState.curDir)
		if action == Quit {
			break
		}
		if action == NewGame {
			gameState = initGameState(width, height, rng)
			continue
		}
		if gameState.GameIsWon || gameState.GameIsLost {
			continue
		}
		gameState = updateGameState(gameState, action, newDir, rng)
	}
}

func getValidInput(playerActions <-chan PlayerAction, curDir direction) (PlayerAction, direction) {
	for {
		select {
		case input := <-playerActions:
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

func updateGameState(gameState GameState, action PlayerAction, newDir direction, r *rand.Rand) GameState {
	if action == Pause {
		gameState.Paused = !gameState.Paused
		return gameState
	}
	if gameState.Paused {
		return gameState
	}
	gameState.curDir = newDir
	snakeLen := len(gameState.Snake)
	newHead := addPointAndDir(gameState.Snake[snakeLen-1], gameState.curDir)
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
