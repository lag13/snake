package snake

import (
	"time"

	"github.com/lag13/snake/Go/controller"
	"github.com/lag13/snake/Go/model"
	"github.com/lag13/snake/Go/view"
)

func Play(gameState *model.GameState, renderer view.Renderer, inputGetter controller.InputGetter, sleepDur time.Duration) int {
	// I think the below structure to the game loop is best:
	// 1. Render (the initial game state has to be drawn after all)
	// 2. Sleep (gives the player some time to see the game and respond to it in its current state)
	// 3. Get input
	// 4. Update
	// If you updated first then the inital game state would never be rendered
	// which feels weird to me. If there was no sleep between the render and
	// the update then people would have less time to respond to what they see
	// on screen before it changes.
	for gameState.GameContinues() {
		renderer.Render(*gameState)
		time.Sleep(sleepDur)
		// TODO: When getting the input maybe keep trying to get input until we
		// get something "valid" for example if they enter two 'h's in a row,
		// the second 'h' is definitely an invalid move so just discard it.
		input := inputGetter.GetInput()
		if input == controller.Quit {
			break
		}
		gameState.UpdateGameState(input)
	}
	return gameState.Score
}
