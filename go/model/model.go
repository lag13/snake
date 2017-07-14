package model

import (
	"math/rand"
	"time"

	"github.com/lag13/snake/Go/controller"
)

type GameState struct {
	Height       int
	Width        int
	Snake        []Pt
	CurDirection Pt
	FoodGen      FoodGenerator
	Food         Pt
	Score        int
	Paused       bool
}

type FoodGenerator interface {
	GenerateFood(height int, width int, snake []Pt) Pt
}

type defaultFoodGenerator struct {
	rng *rand.Rand
}

func buildDefaultFoodGenerator() *defaultFoodGenerator {
	rng := rand.New(rand.NewSource(time.Now().Unix()))
	return &defaultFoodGenerator{rng}
}

func (fg *defaultFoodGenerator) GenerateFood(height int, width int, snake []Pt) Pt {
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

func (gs GameState) GameContinues() bool {
	if snakeOutOfBounds(gs.Height, gs.Width, gs.Snake[0]) {
		return false
	}
	if snakeHitBody(gs.Snake) {
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

func (gs *GameState) UpdateGameState(input int) {
	if input == controller.Pause {
		gs.Paused = !gs.Paused
	}
	if gs.Paused {
		return
	}
	gs.CurDirection = getDirectionFromInput(input, gs.CurDirection)
	oldTail := gs.Snake[len(gs.Snake)-1]
	newSnake := copySnake(gs.Snake)
	newSnake = moveSnake(newSnake, gs.CurDirection)
	newFood := gs.Food
	if newSnake[0] == newFood {
		gs.Score++
		newFood = gs.FoodGen.GenerateFood(gs.Height, gs.Width, newSnake)
		newSnake = append(newSnake, oldTail)
	}
	gs.Food = newFood
	gs.Snake = newSnake
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

func New(height int, width int) *GameState {
	snake := []Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}}
	initialDirection := Pt{0, 1}
	foodGenerator := buildDefaultFoodGenerator()
	food := foodGenerator.GenerateFood(height, width, snake)
	return &GameState{height, width, snake, initialDirection, foodGenerator, food, 0, false}
}
