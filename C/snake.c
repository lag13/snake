#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <time.h>
#include <unistd.h>

typedef struct pos
{
    int y;
    int x;
} pos;

typedef struct pos_list
{
    pos *list;
    int len;
} pos_list;

int positions_are_equal(pos a, pos b)
{
    return a.y == b.y && a.x == b.x;
}

int headHitSnakeBody(pos_list snake)
{
    for(int i = 1; i < snake.len; i++)
    {
        if(positions_are_equal(snake.list[0], snake.list[i]))
        {
            return 1;
        }
    }
    return 0;
}

int pos_list_positionExists(pos_list pl, pos to_find)
{
    for(int i = 0; i < pl.len; i++)
    {
        if(positions_are_equal(to_find, pl.list[i]))
        {
            return 1;
        }
    }
    return 0;
}

pos getNewFoodPosition(pos_list snake, int height, int width)
{
    pos food;
    while(1)
    {
        food.y = rand() % height;
        food.x = rand() % width;
        if(!pos_list_positionExists(snake, food))
        {
            return food;
        }
    }
}

const pos UP = { .y = -1, .x = 0 };
const pos DOWN = { .y = 1, .x = 0 };
const pos LEFT = { .y = 0, .x = -1 };
const pos RIGHT = { .y = 0, .x = 1 };

void initGameState(int height, int width, pos_list *snake, pos *food, pos *movement_dir)
{
    srand(time(NULL));
    snake->list = malloc(sizeof *(snake->list) * height * width);
    snake->list[0].y = 1;
    snake->list[0].x = 1;
    snake->list[1].y = 0;
    snake->list[1].x = 1;
    snake->list[2].y = 0;
    snake->list[2].x = 0;
    snake->list[3].y = 1;
    snake->list[3].x = 0;
    snake->len = 4;
    pos fp = getNewFoodPosition(*snake, height, width);
    *food = fp;
    *movement_dir = DOWN;
    initscr();
    raw();
    noecho();
    timeout(0);
    curs_set(0);
}

void quitGame(pos_list *snake)
{
    while(getch() != ERR);
    free(snake->list);
    endwin();
}

void drawSnake(pos_list snake)
{
    for(int i = 0; i < snake.len; i++)
    {
        mvaddch(snake.list[i].y, snake.list[i].x, ACS_BLOCK);
    }
}

void drawFood(pos food)
{
    mvprintw(food.y, food.x, "@");
}

void drawBorder(int height, int width)
{
    for(int i = 0; i < height+1; i++)
    {
        mvaddch(i, width+1, ACS_VLINE);
    }
    move(height+1, 0);
    for(int i = 0; i < width+1; i++)
    {
        addch(ACS_HLINE);
    }
    addch(ACS_LRCORNER);
}

void drawScore(int start_y, int score)
{
    mvprintw(start_y, 0, "Score: %d", score);
}

void drawGame(pos_list snake, pos food, int height, int width)
{
    drawBorder(height, width);
    drawScore(height+2, snake.len-1);
    drawSnake(snake);
    drawFood(food);
    refresh();
}

void moveSnake(pos_list snake, pos movement_dir)
{
    int last_elem = snake.len-1;
    mvaddch(snake.list[last_elem].y, snake.list[last_elem].x, ' ');
    memmove(snake.list+1, snake.list, (snake.len-1) * sizeof *snake.list);
    snake.list[0].y += movement_dir.y;
    snake.list[0].x += movement_dir.x;
}

void generateFoodPosition(pos *food, pos_list snake, int height, int width)
{
    if(positions_are_equal(snake.list[0], *food))
    {
        pos f = getNewFoodPosition(snake, height, width);
        *food = f;
    }
}

void setMovementDir(pos *movement_dir, int input_char)
{
    if(input_char == 'h' && !positions_are_equal(*movement_dir, RIGHT))
    {
        *movement_dir = LEFT;
    }
    if(input_char == 'j' && !positions_are_equal(*movement_dir, UP))
    {
        *movement_dir = DOWN;
    }
    if(input_char == 'k' && !positions_are_equal(*movement_dir, DOWN))
    {
        *movement_dir = UP;
    }
    if(input_char == 'l' && !positions_are_equal(*movement_dir, LEFT))
    {
        *movement_dir = RIGHT;
    }
}

void updateGameState(pos_list *snake, pos *food, pos *movement_dir, int *should_quit, int input_char, int height, int width)
{
    if(input_char != ERR)
    {
        if(input_char == 'q')
        {
            *should_quit = 1;
            return;
        }
        setMovementDir(movement_dir, input_char);
    }
    pos save_tail_pos = snake->list[snake->len-1];
    moveSnake(*snake, *movement_dir);
    if(positions_are_equal(snake->list[0], *food))
    {
        snake->len += 1;
        snake->list[snake->len-1] = save_tail_pos;
    }
    generateFoodPosition(food, *snake, height, width);
}

int outOfBounds(pos_list snake, int height, int width)
{
    if(snake.list[0].y < 0 || snake.list[0].y > height || snake.list[0].x < 0 || snake.list[0].x > width)
    {
        return 1;
    }
    return 0;
}

int gameContinues(int should_quit, pos_list snake, int height, int width)
{
    if(should_quit)
    {
        return 0;
    }
    if(snake.len == height*width)
    {
        return 0;
    }
    if(snake.len > 1 && headHitSnakeBody(snake))
    {
        return 0;
    }
    if(outOfBounds(snake, height, width))
    {
        return 0;
    }
    return 1;
}

int processInput(int *input, int *cur_input, int *next_input, int *num_input)
{
    int input_size = *num_input;
    for(int i = 0; i < 2-input_size; i++)
    {
        int input_char = getch();
        input[*next_input] = input_char;
        if(input_char != ERR)
        {
            *next_input = (*next_input + 1) % 2;
            (*num_input)++;
        }
    }
    while(getch() != ERR);
    int input_char = input[*cur_input];
    if(input_char != ERR)
    {
        *cur_input = (*cur_input + 1) % 2;
        (*num_input)--;
    }
    return input_char;
}

void gameLoop(pos_list snake, pos food, pos movement_dir, int height, int width)
{
    int input[2];
    int current_input = 0;
    int next_input = 0;
    int should_quit = 0;
    int num_input = 0;
    struct timespec delay = {0, 82500000};
    while(gameContinues(should_quit, snake, height, width))
    {
        drawGame(snake, food, height, width);
        nanosleep(&delay, NULL);
        int input_char = processInput(input, &current_input, &next_input, &num_input);
        updateGameState(&snake, &food, &movement_dir, &should_quit, input_char, height, width);
    }
}

int getBoardDimension(char *str)
{
    int num = atoi(str);
    if(num < 5)
    {
        return 15;
    }
    return num;
}

int main(int argc, char **argv)
{
    int height = 15;
    int width = 15;
    if(argc == 3)
    {
        height = getBoardDimension(argv[0]);
        width = getBoardDimension(argv[1]);
    }
    pos_list snake;
    pos food;
    pos movement_dir;
    initGameState(height, width, &snake, &food, &movement_dir);
    gameLoop(snake, food, movement_dir, height, width);
    quitGame(&snake);
}

