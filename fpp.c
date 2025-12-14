#define _USE_MATH_DEFINES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ncurses.h>
#include <signal.h>
#include <stdbool.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923
#endif

static void finish(int sig);
float ray(float x, float y, float x_step, float y_step, const char *map, int map_width, float view_distance, float ray_resolution);
void raycast_in_fov(float distances[], float player_x, float player_y, float player_angle, float view_distance, const char *map, int map_width, float fov, int screen_resolution, float ray_resolution);
int is_wall(float x, float y, const char *map, int map_width);
void set_steps(float *x_step, float *y_step, float angle, float magnitude);
void render_view(float distances[], int terminal_width, int terminal_height, float retinal_distance, float view_distance);
void player_movement(int direction, float distance, float *player_x, float *player_y, float player_angle, const char *map, int map_width);
void turn(float change, float *player_angle);
void set_starting_loc(float *player_x, float *player_y, const char *map, int map_width, int map_height);
/* Simple one-level map (width includes trailing spaces) */
#define MAP_ROW1  "#####################  "
#define MAP_ROW2  "#...#...............#  "
#define MAP_ROW3  "#...#.........#.....#  "
#define MAP_ROW4  "#...#...#....###....#  "
#define MAP_ROW5  "#.............#.....#  "
#define MAP_ROW6  "#...................#  "
#define MAP_ROW7  "#....###............#  "
#define MAP_ROW8  "#....#..............#  "
#define MAP_ROW9  "#....#....###.......#  "
#define MAP_ROW10 "#..#......###.......#  "
#define MAP_ROW11 "#..#......###.......#  "
#define MAP_ROW12 "#...................#  "
#define MAP_ROW13 "#####################  "
#define MAP_ROW14 "                       "

int main(void)
{
    int term_width = 120;
    int term_height = 55;
    float *distances = (float *)malloc(sizeof(float) * (size_t)term_width);
    if (!distances) return 1;

    const char *map =
        MAP_ROW1
        MAP_ROW2
        MAP_ROW3
        MAP_ROW4
        MAP_ROW5
        MAP_ROW6
        MAP_ROW7
        MAP_ROW8
        MAP_ROW9
        MAP_ROW10
        MAP_ROW11
        MAP_ROW12
        MAP_ROW13
        MAP_ROW14;

    int map_width = (int)(sizeof(MAP_ROW1) - 1);
    int map_height = (int)(strlen(map) / (size_t)map_width);

    /* Parameters */
    float fov = 90.f;
    float view_distance = 3.5f;
    float player_x = 1.5f, player_y = 1.5f; /* no 'S' tile in this simple map */
    float player_a = 0.5f;
    float move_dist = 0.1f;
    float retinal_distance = 0.5f;
    float ray_resolution = 0.001f;
    float key_turn_amt = 0.02f;

    /* ncurses */
    signal(SIGINT, finish);
    initscr();
    resize_term(term_height, term_width);
    keypad(stdscr, TRUE);
    cbreak();
    nodelay(stdscr, TRUE);
    noecho();

    int input;
    bool running = true;
    while (running) {
        input = getch();
        switch (input) {
            case 'w': case 'W': player_movement(0, move_dist, &player_x, &player_y, player_a, map, map_width); break;
            case 'd': case 'D': player_movement(1, move_dist, &player_x, &player_y, player_a, map, map_width); break;
            case 's': case 'S': player_movement(2, move_dist, &player_x, &player_y, player_a, map, map_width); break;
            case 'a': case 'A': player_movement(3, move_dist, &player_x, &player_y, player_a, map, map_width); break;
            case KEY_LEFT: case 'q': turn(-key_turn_amt, &player_a); break;
            case KEY_RIGHT: case 'e': turn( key_turn_amt, &player_a); break;
            case KEY_BACKSPACE: running = false; break;
            default: break;
        }
        raycast_in_fov(distances, player_x, player_y, player_a, view_distance, map, map_width, fov, term_width, ray_resolution);
        render_view(distances, term_width, term_height, retinal_distance, view_distance);
    }

    free(distances);
    finish(SIGINT);
    return 0;
}

void render_view(float distances[], int terminal_width, int terminal_height, float retinal_distance, float view_distance)
{
    float sky_size, projection_height;
    for (int x = terminal_width - 1; x >= 0; x--) {
        projection_height = terminal_height * (retinal_distance / distances[x]);
        for (int y = 0; y < terminal_height; y++) {
            if (distances[x] >= 0) {
                sky_size = (terminal_height - projection_height) * 2.f / 3.f;
                if (y <= sky_size) mvaddch(y, x, ' ');
                else if (y <= (sky_size + projection_height)) {
                    if (distances[x] <= view_distance * 0.5f) mvaddch(y, x, '#');
                    else if (distances[x] <= view_distance * 0.75f) mvaddch(y, x, '%');
                    else mvaddch(y, x, '*');
                }
                else mvaddch(y, x, '.');
            }
            else {
                projection_height = terminal_height * (retinal_distance / (view_distance + 1));
                sky_size = (terminal_height - projection_height) * 2.f / 3.f;
                if (y <= sky_size + projection_height) mvaddch(y, x, ' ');
                else mvaddch(y, x, '.');
            }
        }
    }
    refresh();
}

void raycast_in_fov(float distances[],
                    float player_x, float player_y,
                    float player_angle, float view_distance,
                    const char *map, int map_width,
                    float fov, int screen_resolution,
                    float ray_resolution)
{
    fov = fov / 360.f; /* Convert fov to 0-1 */
    float x_step, y_step;
    float curr_ray_angle = player_angle - (fov / 2);
    float ray_angle_step = fov / (float)screen_resolution;

    for (int i = 0; i < screen_resolution; i++) {
        set_steps(&x_step, &y_step, curr_ray_angle, ray_resolution);
        distances[i] = ray(player_x, player_y, x_step, y_step, map, map_width, view_distance, ray_resolution);
        curr_ray_angle += ray_angle_step;
    }
}

float ray(float x, float y,
          float x_step, float y_step,
          const char *map, int map_width,
          float view_distance, float ray_resolution)
{
    for (float i = ray_resolution; i < view_distance; i += ray_resolution) {
        x += x_step;
        y += y_step;
        if (is_wall((int)x, (int)y, map, map_width) > 0) return i;
    }
    return -1.0f;
}

void turn(float change, float *player_angle)
{
    *player_angle += change;
    if (*player_angle >= 1) *player_angle -= 1;
    else if (*player_angle < 0) *player_angle += 1;
}

void player_movement(int direction, float distance,
                     float *player_x, float *player_y,
                     float player_angle, const char *map,
                     int map_width)
{
    bool valid_move = true;
    float destination_x;
    float hitbox_x;
    float destination_y;
    float hitbox_y;

    switch (direction)
    {
    default:
        set_steps(&destination_x, &destination_y, player_angle, 1.f);
        break;
    case 1:
        set_steps(&destination_x, &destination_y, (player_angle + .25f), 1.f);
        break;
    case 2:
        set_steps(&destination_x, &destination_y, (player_angle + .5f), 1.f);
        break;
    case 3:
        set_steps(&destination_x, &destination_y, (player_angle + .75f), 1.f);
        break;
    }

    hitbox_x = (destination_x * distance * 2.f) + *player_x;
    hitbox_y = (destination_y * distance * 2.f) + *player_y;
    destination_x = (destination_x * distance) + *player_x;
    destination_y = (destination_y * distance) + *player_y;
    valid_move = is_wall(destination_x, destination_y, map, map_width) == 0 && is_wall(hitbox_x, hitbox_y, map, map_width) == 0;

    if (valid_move) {
        *player_x = destination_x;
        *player_y = destination_y;
    }
}

int is_wall(float x, float y, const char *map, int map_width) {
    char c = map[(int)y * map_width + (int)x];
    if (c == '#') {return 1;}
    else if (c == '!') {return 2;}
    else {return 0;}
}

/* bearing helper removed for minimal build */

void set_steps(float *x_step, float *y_step, float angle, float magnitude) {
    if (angle >= .875f) angle -= 1.f;
    float angle_in_rads = 2.f * (float)M_PI * angle; /* Convert to radians */
    angle_in_rads -= (float)M_PI_2;                  /* Shift angle */
    *x_step = cosf(angle_in_rads) * magnitude;
    *y_step = sinf(angle_in_rads) * magnitude;
}

static void finish(int sig) {
    (void)sig;
    endwin();
    exit(0);
}

void set_starting_loc(float *player_x, float *player_y, const char *map, int map_width, int map_height) {
    for (int x = 0; x < map_width; x++) {
        for (int y = 0; y < map_height; y++) {
            if (map[(int)(y) * map_width + (int)(x)] == 'S') {
                *player_x = (float)x + .5f;
                *player_y = (float)y + .5f;
            }
        }
    }
}
