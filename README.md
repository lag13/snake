# Snake
Making the game snake in different programming languages.

## The Game Loop
Games always follow a similar pattern of: "while the game is not over"

1. Render
2. Get input
3. Update the game

This is often referred to as the "game loop". I wanted to write down
my current thoughts on how the "game loop" should be structured and
why:

1. Render the game. We do this first since the initial game state
   should be seen by the player.
2. Sleep. Doing this right after the render phase gives the player a
   maximum amount of time to see the current game state and respond to
   it.
3. Read input.
4. Update the game state based on the input. This should be a "dumb"
   action which doesn't deal with interaction of objects, it just
   moves objects to a new state.
5. Resolve interactions between objects.

Originally I was thinking that somewhere before or after (1) you
should try and resolve interactions between objects. My thinking was
that the initial game state could have interactions that need
resolving. But I think that just overcomplicates things. The intial
configuration of the game state should be set up so there are
initially no interactions.

(4) and (5) will probably be meshed together but it is worth
considering them separately as it can help when thinking about the
problem.

## TODO:
1. There still is some refactoring I want to do for the C
   implementation. In particular:
   - Right now we rely on ncurses ability to get input or return ERR
     if no input is available but I think the logic would be more
     portable if we get input in a separate thread and add it to a
     shared queue. So do this.
   - Consider writing a little .org file in the C directory for tips
     on developing C code.
   - I currently made one big header file which contains "snake
     utilities" but it could be broken up into multiple files.
   - Right now I wait for a keypress before quitting out of the game,
     but oftentimes a key is hit immediately so they don't get to see
     the end game score and such. This could get when I change the
     input handling to use a queue but just keep in mind that I want
     them to C (C what I did there? (C what I did there? (infinite
     recursion... ))) the end of game scenario.
