package snakegame

// TODO: Add documentation

// TODO: Should I make an abstract snake type and food type? And give
// functions to help draw them? That way the renderer does not have to worry
// about their internal implementation (which they do now, they know that they
// are Pt's)

import (
	"math/rand"
	"time"
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
	Sleeper       Sleeper
}

type InputGetter interface {
	GetInput() rune
}

type defaultInputGetter struct {
	lastInput rune
	in        <-chan rune
}

// TODO: Make this only accept input that is "valid"
// TODO: Should this be called something like humanInputGetter?
func (g *defaultInputGetter) GetInput() rune {
	select {
	case g.lastInput = <-g.in:
	default:
	}
	return g.lastInput
}

// TODO: We I'm passing in 'j' here so we move in the same direction as
// dictated by curDirection. I shouldn't have to do this. Do we need
// curDirection at all? Could the direction we're travelling in be stored
// here? I could call it inputHandler instead and it will get input AND return
// the appropriate direction?
func buildDefaultInputGetter(inputStream <-chan rune) *defaultInputGetter {
	return &defaultInputGetter{'j', inputStream}
}

type Renderer interface {
	Render(gs GameState)
}

type Sleeper interface {
	Sleep()
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

type defaultSleeper struct {
	duration time.Duration
}

func (s defaultSleeper) Sleep() {
	time.Sleep(s.duration)
}

func buildDefaultSleeper() Sleeper {
	return defaultSleeper{100 * time.Millisecond}
}

type Pt struct {
	X int
	Y int
}

func GameContinues(height int, width int, snake []Pt) bool {
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

func UpdateGameState(gs GameState) (newGameState GameState) {
	oldTail := gs.Snake[len(gs.Snake)-1]
	newSnake := copySnake(gs.Snake)
	newSnake = moveSnake(newSnake, gs.curDirection)
	newFood := gs.Food
	if newSnake[0] == newFood {
		newFood = gs.foodGenerator.generateFood(gs.Height, gs.Width, newSnake)
		newSnake = append(newSnake, oldTail)
	}
	// TODO: I don't think that I like how I'm returning a new game state
	// object but the only two things that change are the snake and food
	// postitions. Fix this, make it nicer. I feel doing this could also
	// relate to how I don't like how big the GameState struct is.
	return GameState{gs.Height, gs.Width, newSnake, gs.curDirection, gs.foodGenerator, newFood, gs.inputGetter, gs.Sleeper}
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

// TODO: Make these constants?
var (
	left  = Pt{-1, 0}
	down  = Pt{0, 1}
	up    = Pt{0, -1}
	right = Pt{1, 0}
)

var inputToDirectionMap = map[rune]Pt{
	'h': left,
	'j': down,
	'k': up,
	'l': right,
}

func GetDirectionFromInput(input rune, currentDirection Pt) Pt {
	dir, ok := inputToDirectionMap[input]
	if !ok {
		notOrthogonalToPossibleDirs := Pt{1, 1}
		dir = notOrthogonalToPossibleDirs
	}
	if pointsAreOrthogonal(dir, currentDirection) {
		return dir
	}
	return currentDirection
}

func pointsAreOrthogonal(p1 Pt, p2 Pt) bool {
	return p1.X*p2.X+p1.Y*p2.Y == 0
}

func InitSnakeGame(height int, width int, inputStream <-chan rune) GameState {
	snake := []Pt{{1, 1}, {1, 0}, {0, 0}, {0, 1}}
	initialDirection := Pt{0, 1}
	foodGenerator := buildDefaultFoodGenerator()
	food := foodGenerator.generateFood(height, width, snake)
	inputGetter := buildDefaultInputGetter(inputStream)
	sleeper := buildDefaultSleeper()
	return GameState{height, width, snake, initialDirection, foodGenerator, food, inputGetter, sleeper}
}

func GameLoop(r Renderer, gs GameState) {
	for GameContinues(gs.Height, gs.Width, gs.Snake) {
		r.Render(gs)
		gs.Sleeper.Sleep()
		input := gs.inputGetter.GetInput()
		gs.curDirection = GetDirectionFromInput(input, gs.curDirection)
		gs = UpdateGameState(gs)
	}
}
