command! Snake call s:snake()

function! s:snake()
    let initGameState = s:initGame()
    call s:gameLoop(initGameState)
    call s:quitGame()
endfunction

function! s:initGame()
    call s:openEmptyCanvas()
    let initGameState = s:initGameState()
    call s:drawBoard(initGameState["height"], initGameState["width"])
    return initGameState
endfunction

function! s:openEmptyCanvas()
    call s:openNewTab()
    call s:maximizeScreenSpace()
endfunction

function! s:openNewTab()
    let g:save_tabnr = tabpagenr()
    tabedit
endfunction

function! s:maximizeScreenSpace()
    setlocal nonumber
    setlocal nocursorline
    setlocal nocursorcolumn
    setlocal nowrap
    let g:save_laststatus = &laststatus
    let g:save_showtabline = &showtabline
    set laststatus=0
    set showtabline=0
endfunction

function! s:initGameState()
    call s:seedRNG(localtime())
    let height = min([15, &lines-1])
    let width = min([15, &columns])
    let snake_body = [[1, 1]]
    return s:newGameState(height, width, snake_body, [1, 0], s:generateFoodPos(snake_body, height, width))
endfunction

function! s:seedRNG(seed)
    let g:seed = a:seed % 509
endfunction

function! s:newGameState(height, width, snake_body, cur_dir, food_pos)
    return {
                \ "height": a:height,
                \ "width": a:width,
                \ "snake_body": a:snake_body,
                \ "cur_dir": a:cur_dir,
                \ "food_pos": a:food_pos,
                \ }
endfunction

function! s:drawBoard(height, width)
    for y in range(1, a:height)
        call setline(y, repeat(' ', a:width) . '|')
    endfor
    call setline(a:height+1, repeat('-', a:width) . '+')
    " This emtpy line will be where the score goes
    normal! Go
    redraw
endfunction

function! s:gameLoop(gameState)
    let gs = a:gameState
    while !s:gameShouldEnd(gs["cur_dir"], gs["height"], gs["width"], gs["snake_body"])
        call s:drawGame(gs["height"], gs["width"], gs["snake_body"], gs["food_pos"])
        let gs = s:updateGameState(gs)
        sleep 90ms
    endwhile
endfunction

function! s:gameShouldEnd(new_dir, height, width, snake_body)
    return s:playerQuit(a:new_dir) || s:gameOver(a:height, a:width, a:snake_body)
endfunction

function! s:playerQuit(new_dir)
    return a:new_dir ==# [0, 0]
endfunction

function! s:gameOver(height, width, snake_body)
    if s:headHitTail(a:snake_body) || s:outOfBounds(a:snake_body[0], a:height, a:width)
        return 1
    endif
    return 0
endfunction

function! s:headHitTail(snake_body)
    let i = 1
    while i < len(a:snake_body)
        if a:snake_body[0] == a:snake_body[i]
            return 1
        endif
        let i += 1
    endwhile
    return 0
endfunction

function! s:outOfBounds(snake_head, height, width)
    return a:snake_head[0] < 1 || a:snake_head[0] > a:height || a:snake_head[1] < 1 || a:snake_head[1] > a:width
endfunction

function! s:drawGame(height, width, snake_body, food_pos)
    call s:clearBoard(a:height, a:width)
    call s:drawSnake(a:snake_body)
    call s:drawFood(a:food_pos)
    call s:drawScore(a:snake_body)
    " When I didn't have this call the cursor was on top of the score which
    " made it hard to read
    call cursor(1, 1)
    redraw
endfunction

function! s:clearBoard(height, width)
    call cursor(1, 1)
    execute "normal! \<C-v>".(a:width-1)."l".(a:height-1)."jr "
endfunction

function! s:drawSnake(snake_body)
    for s in a:snake_body
        call s:drawChar('#', s)
    endfor
endfunction

function! s:drawChar(char_to_draw, pos)
    call cursor(a:pos)
    execute "normal r".a:char_to_draw
endfunction

function! s:drawFood(food_pos)
    call s:drawChar('@', a:food_pos)
endfunction

function! s:drawScore(snake_body)
    execute "normal! GccScore: ".(len(a:snake_body)-1)."\<ESC>"
endfunction

function! s:updateGameState(gameState)
    let gs = a:gameState
    let new_dir = s:getDirectionFromInput(gs["cur_dir"])
    let new_snake_body = s:getNewSnakeBody(gs["snake_body"], gs["food_pos"], new_dir)
    let new_food_pos = s:getNewFoodPos(gs["snake_body"], gs["food_pos"], new_snake_body, gs["height"], gs["width"])
    return s:newGameState(gs["height"], gs["width"], new_snake_body, new_dir, new_food_pos)
endfunction

" For the most part this game was working fine but occasionally I would run
" into an issue where my input didn't seem to register with the game. For
" example, I would hit 'j' then 'l' in quick succession and the 'j' would
" register but the 'l' wouldn't. I'm not sure how other games do it (perhaps
" they have a dedicated thread which continuously reads user input) but this
" solution seems to work more often than not. What I do is keep consuming a
" character from the input until I get something meaningful to the game or
" until it is exhausted (at which point I just return the current direction of
" travel). 'Meaningful' in this context is a direction perpendicular to the
" current direction of travel. I still sometimes run into problems where it
" seems my keystrokes don't register. I'm still unsure of the reason for that.
function! s:getDirectionFromInput(cur_dir)
    let new_dir = [1, 1]
    while 1
        let input = s:getInput()
        if input ==# ''
            return a:cur_dir
        endif
        let new_dir = s:inputToDir(input, new_dir)
        " The only valid type of movement in snake is to move orthogonally to
        " the direction of travel, if that is not the movement entered then we
        " keep consuming input.
        if s:isOrthogonalMovement(new_dir, a:cur_dir)
            return new_dir
        endif
    endwhile
endfunction

function! s:getInput()
    let c = getchar(0)
    if c == 0
        return ''
    else
        return nr2char(c)
    endif
endfunction

function! s:inputToDir(input, default)
    let left = [0, -1]
    let down = [1, 0]
    let up = [-1, 0]
    let right = [0, 1]
    let mapping = {
                \ 'h': left,
                \ 'j': down,
                \ 'k': up,
                \ 'l': right,
                \ 'w': up,
                \ 's': down,
                \ 'a': left,
                \ 'd': right,
                \ 'q': [0, 0]
                \ }
    return get(mapping, a:input, a:default)
endfunction

function! s:isOrthogonalMovement(new_dir, old_dir)
    return s:dotProduct(a:new_dir, a:old_dir) == 0
endfunction

function! s:dotProduct(v1, v2)
    let result = 0
    for i in range(0, len(a:v1)-1)
        let result += a:v1[i] * a:v2[i]
    endfor
    return result
endfunction

function! s:getNewSnakeBody(snake_body, food_pos, new_dir)
    let new_snake_body = deepcopy(a:snake_body)
    if s:snakeAteFood(a:snake_body, a:food_pos)
        call add(new_snake_body, [0, 0])
    endif
    return s:moveSnake(new_snake_body, a:new_dir)
endfunction

function! s:snakeAteFood(snake_body, food_pos)
    return a:snake_body[0] ==# a:food_pos
endfunction

function! s:moveSnake(snake_body, new_dir)
    let new_snake_body = a:snake_body
    let i = len(new_snake_body)-1
    while i > 0
        let new_snake_body[i] = new_snake_body[i-1]
        let i = i - 1
    endwhile
    let new_snake_body[0] = s:addVector(new_snake_body[0], a:new_dir)
    return new_snake_body
endfunction

function! s:addVector(v1, v2)
    let v3 = []
    for i in range(0, len(a:v1)-1)
        call add(v3, a:v1[i] + a:v2[i])
    endfor
    return v3
endfunction

function! s:getNewFoodPos(old_snake_body, old_food_pos, new_snake_body, height, width)
    if !s:snakeAteFood(a:old_snake_body, a:old_food_pos)
        return a:old_food_pos
    endif
    return s:generateFoodPos(a:new_snake_body, a:height, a:width)
endfunction

" TODO: Should we put the code in here which detects if it is or is not
" possible to generate more food? Or somewhere else? Remember, when you can't
" generate anymore food then you've won.
" TODO: Improve this code in the future. I feel this is kind of brute force
" right now (i.e just keep generating until it works). We could build a list
" of available places for food and then randomly generate an index into that
" list.
function! s:generateFoodPos(snake_body, height, width)
    while 1
        let pos = s:genRandomPos(a:height, a:width)
        if index(a:snake_body, pos) == -1
            return pos
        endif
    endwhile
endfunction

function! s:genRandomPos(height, width)
    let y = s:rand() % a:height + 1
    let x = s:rand() % a:width + 1
    return [y, x]
endfunction

function! s:rand()
    let a = 35
    let c = 1
    let m = 509
    let g:seed = (a * g:seed + c) % m
    return g:seed
endfunction

function! s:quitGame()
    let &laststatus = g:save_laststatus
    let &showtabline = g:save_showtabline
    bdelete!
    execute "tabnext ".g:save_tabnr
endfunction
