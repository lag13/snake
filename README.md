Making sure emacs can push to github too.

# Snake
Making the game snake in different programming languages.

## The Game Loop
Games (probably) always follow a similar pattern of: "while the game
is not over"

1. Render
2. Get input
3. Update the game

This is often referred to as the "game loop". I wanted to write down
my current thoughts on how the "game loop" should be ordered and why:

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

## About Input
Thoughts about how to deal with input and the different "kinds" of
input.

### Kinds of Inputs
It sort of feels like there are 2 "kinds" of inputs when dealing with
a game:

1. Inputs which are sort of "outside" any individual game. For example
   the "on/off" and "reset" buttons on a nintendo 64.
   Starting/quitting and restarting a game feel like the only possible
   inputs in this category.
2. Inputs that happen while the game is active. This includes "pause".

Not super important but I just wanted to keep this in mind. Perhaps
some sort of library could be made to handle (1) so other games need
only worry about (2).

### Different "Ways" Inputting
Wanted to jot down all the different "ways" input could be input so I
could start thinking about how you would handle these different
scenarios:

1. One single button press.
2. Two or more buttons pressed simultaneously.
3. Holding down one or more buttons.
4. A sequence of buttons pressed in "quick" succession. (like tapping
   'A' three times in super smash bros. often does a punch,punch,kick
   sort of action).

In the list above I would also consider an anaolog stick to be a
"button" (it feels like it would be handled in a similar manner to
buttons in any case).

### Consuming Input
#### Game Logic Should Define Input Types
The game logic should define a type and update the state based on
values of that type as opposed to making decisions based off "raw"
inputs like, "if 'w' then move up". The benefit to doing this is that
the core game logic is agnostic to where input is coming from and
would allow things like different controller configurations for the
same game. For snake there could be an enum with values: UP, DOWN,
LEFT, RIGHT, etc...

If the game loop is part of the game logic package it can read from
thread safe queue of those types. In that case you'll need to create
some thread in main which populates that queue with values.

#### Inspiration From Programming Languages
##### EDIT
I've since concluded that this approach of having a separate
package/module/thread which filters out invalid move sequences so the
core game logic only recieves valid move sequences doesn't really work
as nicely as I would have thought/hoped. But I want to keep this
record of my thought process and I think this pattern could still be
utilized in certain scenarios. Essentially, parsing a programming
language seems to consist of these functions:

1. `String -> [Token]`
2. `[Token] -> Either Error ParseTree`

I thought I could have a similar set of functions for games:

1. `RawInput -> GameMove`
2. `GameMove -> Maybe OnlyValidGameMove`

And then the game logic could have an update function like this:
`OnlyValidGameMove -> GameState -> GameState`. The problem is that
where function (2) for language parsing only needs `[Token]` to
determine validity of the stream of tokens and make a parse tree, the
function (2) for games also needs to know the game state in order to
determine if a move is valid or not. For example in Super Mario 64 you
press 'A' to jump, but if you're in the air (which is game state) then
hitting 'A' won't make you jump. Or you could be standing there doing
nothing and get killed by an enemy (being "dead" is also game state)
in which case you cannot take the same actions you normally do. So the
two sets of functions really become:

1. `RawInput -> GameMove`
2. `GameMove -> GameState -> Maybe OnlyValidGameMove`

And now we see that since (2) AND the update function
(`OnlyValidGameMove -> GameState -> GameState`) both need to reference
the game state it doesn't really make sense to have them as separate
modules. You *could* try to do this but it seems overly complicated
(you'd probably need some mutex around the game state so (2) can
inspect it without it changing among other things) and so this
"filtering out invalid move sequences" really belongs inside the core
game logic and not as some separate module.

##### My Original Thoughts
Dealing with input in this fashion seems nice:

1. Have one layer which translates the "raw input" into a data type
   which the game understands.
2. Have another layer which filters out input sequences that are not
   possible. With snake for instance inputting a direction which is
   the opposite or the same as the one in which you're travelling does
   not make sense.
3. Now the "core game logic" will only recieve "valid" inputs which
   makes the logic simpler.

I believe this layered approach parellels what is often done when
parsing a language. I've never written a language myself (so I could
be wrong) but I believe a common approach is:

1. Write a "lexer" which takes the "raw" stream of characters and
   outputs a stream of "tokens" which are keywords/symbols that are
   meaningful to the language like "if", "{", and "func".
2. Write a "parser" which takes a stream of tokens and makes sure that
   the sequence of tokens makes sense (in C for instance a "}"
   followed by a "{" does not make sense). This stage outputs a "tree"
   of those tokens (assuming everything made sense).
3. Now an evaluator can traverse and evaluate that tree. This is your
   running program.

To relate this to human language:

- lexer - "stream of characters" -> "stream of words"
- parser - "stream of words" -> "stream of valid sentences"
- evaluator - "stream of valid sentences" -> "meaning"

## TODO:
1. If you enter up-down-left-right (or some other combination) do
   something fun like display a message or make you win the game.
