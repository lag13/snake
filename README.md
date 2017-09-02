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

## On Getting Input
It feels like a good idea to handle input in this fashion:

1. Have one layer which translates the "raw input" into a data type
   which the game understands.
2. Have another layer which filters out input sequences that are not
   possible. With snake for instance inputting a direction which is
   the opposite or the same as the one in which you're travelling does
   not make sense.
3. Now the "core game logic" will only recieve "valid" inputs which
   makes the logic simpler.

I believe this layered approach parellels what is often done when
making a compiled language. I've never written one myself (so I could
be wrong) but I believe a common approach is:

1. Write a "lexer" which takes the "raw" stream of characters and
   outputs a stream of "tokens" which are keywords/symbols meaningful
   to the language like "if", "{", and "func".
2. Write a "parser" which takes a stream of tokens and makes sure that
   the sequence of tokens makes sense (in golang for instance a "}"
   followed by a "{" does not make sense). This stage outputs a "tree"
   of those tokens (assuming everything made sense).
3. Now an evaluator can traverse and evaluate that tree. This is your
   running program.

To relate this to grammar:
- lexer - "stream of characters" -> "stream of words"
- parser - "stream of words" -> "stream of valid sentences"
- evaluator - "stream of valid sentences" -> "meaning"

### Kinds Of Input
Wanted to jot down all the different "types" of input I could think of
which a game could accept so I could start thinking about how you
would handle these different scenarios:

1. One single button press.
2. Two or more simultaneous button presses.
3. Holding down one or multiple buttons.
4. A sequence of buttons pressed in "quick" succession.

In the list above I would also consider an anaolog stick to be a
"button" (it feels like that would be handled in a similar manner to
buttons).

### Thoughts
My goal right now is to:
1. Make it so the game loop only recieves inputs which are "valid".
   This way the game loop can just take what it is given and act on it
   without any second thought. For example, if the snake is travelling
   "up" then the game loop should never even recieve an "up" or "down"
   input.
2. Make documentation generation automatic (but this is secondary to
   1).

I'm not sure if trying to attain this goal is possible/practical but
it feels like it would be nice to achieve because future games could
benefit from it.

First off, there is no question that we will have a queue of "raw"
input which holds the player input as this allows us to be agnostic
about where this input comes from. There is also no question that
we'll have a queue of values that the game understands. So what's left
to do is to figure out what the service that translates from "raw
inputs" to "game values" will look like.

#### Terminology
- RI - raw input (which comes directly from the player).
- SI - structured input (values defined by the game logic like UP and
  DOWN constants).
- keymap - A mapping RI -> SI. For example, 'w' -> UP.
- TS - translation service, this is what is going to translate
  queue(RI) -> queue(SI).

My thoughts on how to achive this:

#### Create a Static Keymap
##### Conclusion
This might be the way to go with snake since there are no
"combination" key presses that can take place. In general though it
might be too simplistic.

##### Thoughts
This is the simple option which doesn't achieve (1) but its still
worth considering.

Pros:
- Simple to implement and understand the purpose of this translation
  layer.
- It would be easy to auto-generate documentation for snake since the
  keymap is simple.

Cons:
- The game can still recieve "invalid" input sequences. For example it
  can recieve two UP's in a row which means the game loop is
  responsible for filtering out the duplicate UP's. Maybe this is fine
  though! After all the game logic would probably most familiar with
  what those inputs are supposed to mean.
- For more complicated input sequences (like when you hit a-a-a in
  rapid succesion in super smash bros which does a punch-punch-kick
  sort of thing for a lot of characters) I don't think you'd be able
  to auto-generate documentation because I'd imagine that the second
  and third 'a' doing different things would be encoded somewhere in
  the game loop.

#### Have Multiple Keymaps
##### Conclusion
I've decided that this approach doesn't quite work because it makes
the assumption that what keystrokes you are allowed to press is wholly
determined by what has been pressed previously which is simply not
true. The game state could change on its own independent of player
input and that could cause a change in what you are "allowed" to
press.

##### Thoughts
I'm taking one from vim's book here. Have a different keymap depending
on what "mode" you're in. The keys entered could end up causing a mode
switch AND produce a SI for the game to consume. I'm not exactly sure
how it'd be implemented but here are my thoughts on what the keymap
would look like for snake:

Modes:
- VERTICAL
- HORIZONTAL
- PAUSED

SI:
- UP
- DOWN
- LEFT
- RIGHT
- PAUSE
- NEW_GAME
- QUIT_GAME

Keymaps:
- VERTICAL
  - 'a' -> (LEFT, HORIZONTAL)
  - 'd' -> (RIGHT, HORIZONTAL)
  - 'p' -> (PAUSE, PAUSED)
  - 'n' -> (NEW_GAME, <starting-mode-for-game>)
  - 'q' -> (QUIT_GAME, ???(I guess it doesn't matter)???)
- HORIZONTAL
  - 'w' -> (UP, VERTICAL)
  - 's' -> (DOWN, HORIZONTAL)
  - 'p' -> (PAUSE, PAUSED)
  - 'n' -> (NEW_GAME, <starting-mode-for-game>)
  - 'q' -> (QUIT_GAME, ???(I guess it doesn't matter)???)
- PAUSED
  - 'p' -> (PAUSE, <previous-active-mode>)
  - 'n' -> (NEW_GAME, <starting-mode-for-game>)
  - 'q' -> (QUIT_GAME, ???(I guess it doesn't matter)???)

Pros:
- Accomplishes (1) in that the game will only recieve valid input
  sequences which makes the game logic simpler.
- It would be more complicated logic but I feel like you can
  auto-generate some documentation for more complicated inputs like a
  sequence of inputs which do something.

Cons:
- Still haven't worked out the details of how *exactly* this would be
  implemented (the "pause" and "quit" states for instance).
- There is a good degree of coupling between the game logic/state and
  the TS. For example, if the game starts out in a "paused" state but
  we forget to update the TS to also start in a PAUSED state then
  we'll have problems because all you can do is press 'p' which would
  put the game into a "non-paused" state but put the TS into a PAUSED
  state. So if the game is not working it might be tricky to figure
  out where the problem is exactly. In general it just feels like some
  "modes" are very closely related to "game state" and that worries
  me. Another example is in banjo kazooie you can jump and then press
  A again to fly. With this approach I'd picture 'A' moving you to a
  new "IN_THE_AIR" keymap and when you are in the air another 'A'
  would make you fly. But this doesn't really seem to work because
  eventually you will fall and hit the ground so you are no longer
  "IN_THE_AIR". The game logic would know that but I don't know how
  that would be communicated with the TS. Maybe this is okay though?
  Maybe there's still ways to make this work? Some "acceptance tests"
  could probably help but its still concerning. At least with the
  first approach if things are going wrong then you KNOW its probably
  the game logic that's at fault since the TS is so dead simple.

#### Fusion of the Previous 2
After thinking about the previous 2 options, I think the best general
approach might be sort of a fusion of the two. First off, the game
loop can recieve a sequence of "invalid" moves and it is responsible
for filtering those out. Secondly, I think there could be value in
having multiple keymaps inside the game loop based on the game state.
So basically we just want to move more logic into the game loop
because having them separated does not seem feasible.

The library/device we are using to input characters is responsible for
mapping their inputs to inputs the game understands. So for snake
they'll have a mapping like:

'w' -> UP
's' -> DOWN
'a' -> LEFT
'd' -> RIGHT
'p' -> PAUSE
'n' -> NEW_GAME
'q' -> QUIT

Then in the snake game we will have a mapping of "states"/"modes" to
what actions are possible. These actions should have help text so we
can generate help documentation and the mode this action puts us in.
So you can visualize the mapping as: `mode -> [(game-action, new-mode,
description)]`

- VERTICAL
  - (LEFT, HORIZONTAL, "move the snake left")
  - (RIGHT, HORIZONTAL, "move the snake right")
  - (PAUSE, PAUSED, "pause the game")
  - (NEW_GAME, VERTICAL, "start a new game")
  - (QUIT_GAME, does-not-matter?, "quit the game")
- HORIZONTAL
  - (UP, VERTICAL, "move the snake up")
  - (DOWN, VERTICAL, "move the snake down")
  - (PAUSE, PAUSED, "pause the game")
  - (NEW_GAME, VERTICAL, "start a new game")
  - (QUIT_GAME, does-not-matter?, "quit the game")
- PAUSED
  - (PAUSE, PAUSED, "pause the game")
  - (NEW_GAME, VERTICAL, "start a new game")
  - (QUIT_GAME, does-not-matter?, "quit the game")

This is basically a graph of possible actions. My rough thinking on
the algorithm for traversing this and creating complete help
documentation would be:
- If a particular tuple appears in all modes then that is a "global"
  action and should probably be listed at the top of the help
  documentaion (i.e PAUSE, NEW_GAME, QUIT_GAME)
- For moves that appear in only one mode, generate help text for the
  mode and then under that the help text for those specific moves.
  This means each mode needs to have a toString() method.
- I'm not sure yet how you'd traverse this data structure (or if I've
  even got this data structure right) to generate help documentation
  for combinations of keys but I think it could be done.

For this initial go through I'll probably ignore the "state"/"mode"
stuff and just have one keymap since I want to see this help doc
generation working and because snake is simple. So we'll just have
something like:
- UP -> "move the snake up"
- DOWN -> "move the snake down"
- LEFT -> "move the snake left"
- RIGHT -> "move the snake right"
- PAUSE -> "pause the game"
- NEW_GAME -> "start a new game"
- QUIT_GAME -> "quit the game"

And the game will be responsible for filtering out invalid sequences
of inputs. To generate the complete help documentation the
library/device which does the conversion to in game values like UP
must also provide a toString() for the inputs it translates. So it
will have a keymap like the one I specified above ('w' -> UP and all
that) but each "key" must be convertible to a string. Then with that
keymap and the one in the game you can generate the help
documentation. Functionally it would probably look something like
this:

(Show a) -> Map (a -> in-game-input) -> Map (in-game-input -> String) -> String.

So you give it a mapping of some key 'a' which can be converted to a
string to the in game input and you give it the mapping of the in game
input to a String (which is the help text) and then it outputs one big
string which is the complete documentation. For C though I'm not sure
how you'd program this so maybe I'll simplify it and have it be just:

Map (Char -> in-game-input) -> Map (in-game-input -> String) -> String

## TODO:
1. C implementation:
   - Right now we rely on ncurses ability to get input or return ERR
     if no input is available but I think the logic would be more
     portable if we get input in a separate thread and add it to a
     shared queue. I think it will also be interesting to experiment
     with removing the "current direction" variable and instead just
     always have a direction on this queue. So if the queue only has
     one direction on it then that direction stays in the queue.
   - Right now I wait for a keypress before quitting out of the game,
     but oftentimes a key is hit immediately so they don't get to see
     the end game score and such. This could be fixed when I change
     the input handling to use a queue but just keep in mind that I
     want them to C (C what I did there? (C what I did there?
     (infinite recursion... ))) the end of game scenario.
   - Currently if you enter a key for the same direction that you are
     travelling in then it will be consumed by the game so any other
     input you had will have to wait.
   - Add a "quit" ('q' key) ability and a "new game" ('n' key)
     ability.
   - I should probably display the controls so people can see them.
2. If you enter up-down-left-right (or some other combination) do
   something fun like display a message or make you win the game.
