package model

// TODO: Add documentation

// TODO: Should I make an abstract snake type and food type? And give
// functions to help draw them? That way the renderer does not have to worry
// about their internal implementation (which they do now, they know that they
// are Pt's)

import (
	"math/rand"
	"time"

	"github.com/lag13/snake/Go/controller"
)

// TODO: I'm not sure if I like this big struct of stuff. Can we break it up?
// Or change how we do things so this is not needed?
type GameState struct {
	Height        int
	Width         int
	Snake         []Pt
	curDirection  Pt
	foodGenerator foodGenerator
	Food          Pt
	inputGetter   InputGetter
	sleepDuration time.Duration
	Score         int
	Paused        bool
}

type InputGetter interface {
	GetInput() int
}

type Renderer interface {
	Render(gs GameState)
}

type foodGenerator interface {
	generateFood(height int, width int, snake []Pt) Pt
}

type defaultFoodGenerator struct {
	rng *rand.Rand
}

func buildDefaultFoodGenerator() *defaultFoodGenerator {
	rng := rand.New(rand.NewSource(time.Now().Unix()))
	return &defaultFoodGenerator{rng}
}

func (fg *defaultFoodGenerator) generateFood(height int, width int, snake []Pt) Pt {
	possiblePositions := getPossiblePositions(height, width, snake)
	if len(possiblePositions) == 0 {
		return Pt{-1, -1}
	}
	return possiblePositions[fg.rng.Intn(len(possiblePositions))]
}

type Pt struct {
	X int
	Y int
}

func gameContinues(height int, width int, snake []Pt) bool {
	if snakeOutOfBounds(height, width, snake[0]) {
		return false
	}
	if snakeHitBody(snake) {
		return false
	}
	return true

}
func snakeOutOfBounds(height int, width int, snakeHead Pt) bool {
	if snakeHead.Y < 0 || snakeHead.X < 0 {
		return true
	}
	if snakeHead.Y >= height || snakeHead.X >= width {
		return true
	}
	return false
}

func snakeHitBody(snake []Pt) bool {
	for i := 1; i < len(snake); i++ {
		if snake[0] == snake[i] {
			return true
		}
	}
	return false
}

func getPossiblePositions(height int, width int, snake []Pt) []Pt {
	availablePositions := []Pt{}
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			pt := Pt{x, y}
			if !positionExists(pt, snake) {
				availablePositions = append(availablePositions, pt)
			}
		}
	}
	return availablePositions
}

func positionExists(ptToCheck Pt, pts []Pt) bool {
	for _, pt := range pts {
		if ptToCheck == pt {
			return true
		}
	}
	return false
}

func updateGameState(gs GameState) (newGameState GameState) {
	oldTail := gs.Snake[len(gs.Snake)-1]
	newSnake := copySnake(gs.Snake)
	newSnake = moveSnake(newSnake, gs.curDirection)
	newFood := gs.Food
	if newSnake[0] == newFood {
		gs.Score++
		newFood = gs.foodGenerator.generateFood(gs.Height, gs.Width, newSnake)
		newSnake = append(newSnake, oldTail)
	}
	// TODO: I don't think that I like how I'm returning a new game state
	// object but the only two things that change are the snake and food
	// postitions. Fix this, make it nicer. I feel doing this could also
	// relate to how I don't like how big the GameState struct is.
	return GameState{gs.Height, gs.Width, newSnake, gs.curDirection, gs.foodGenerator, newFood, gs.inputGetter, gs.sleepDuration, gs.Score, gs.Paused}
}

func copySnake(snake []Pt) []Pt {
	newSnake := make([]Pt, len(snake))
	copy(newSnake, snake)
	return newSnake
}

func moveSnake(snake []Pt, direction Pt) []Pt {
	for i := len(snake) - 1; i > 0; i-- {
		snake[i] = snake[i-1]
	}
	snake[0].X += direction.X
	snake[0].Y += direction.Y
	return snake
}

var (
	left  = Pt{-1, 0}
	down  = Pt{0, 1}
	up    = Pt{0, -1}
	right = Pt{1, 0}
)

var inputToDirectionMap = map[int]Pt{
	controller.Left:  left,
	controller.Down:  down,
	controller.Up:    up,
	controller.Right: right,
}

func getDirectionFromInput(input int, currentDirection Pt) Pt {
	dir, ok := inputToDirectionMap[input]
	if !ok {
		return currentDirection
	}
	if pointsAreOrthogonal(dir, currentDirection) {
		return dir
	}
	return currentDirection
}

func pointsAreOrthogonal(p1 Pt, p2 Pt) bool {
	return p1.X*p2.X+p1.Y*p2.Y == 0
}

func InitSnakeGame(height int, width int, sleep int, inputGetter InputGetter) GameState {
	snake := []Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}}
	initialDirection := Pt{0, 1}
	// TODO: Instead of doing all this "buildDefault" stuff could we just make
	// a new struct type containing these "default" things then define the 0
	// value of those struct members to be the result of these functions? This
	// might be able to help in this issue and others:
	// https://peter.bourgon.org/go-best-practices-2016/#program-design
	foodGenerator := buildDefaultFoodGenerator()
	food := foodGenerator.generateFood(height, width, snake)
	return GameState{height, width, snake, initialDirection, foodGenerator, food, inputGetter, time.Duration(sleep) * time.Millisecond, 0, false}
}

func GameLoop(r Renderer, gs GameState) int {
	// I think this structure to the game loop is best:
	// 1. Render (the initial game state has to be drawn after all)
	// 2. Sleep (gives the player some time to see the game and respond to it in its current state)
	// 3. Get input
	// 4. Update
	// If you updated first then the inital game state would never be rendered
	// which feels weird to me. If there was no sleep between the render and
	// the updating then people would have less time to respond to what they
	// see on screen before it changes.
	for gameContinues(gs.Height, gs.Width, gs.Snake) {
		r.Render(gs)
		time.Sleep(gs.sleepDuration)
		// TODO: When getting the input maybe keep trying to get input until we
		// get something "valid" for example if they enter two 'h's in a row,
		// the second 'h' is definitely an invalid move so just discard it.
		input := gs.inputGetter.GetInput()
		if input == controller.Quit {
			break
		}
		if input == controller.Pause {
			gs.Paused = !gs.Paused
		}
		if !gs.Paused {
			gs.curDirection = getDirectionFromInput(input, gs.curDirection)
			gs = updateGameState(gs)
		}
	}
	return gs.Score
}
