#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct lab_t {
    int size;
    int start_row;
    int start_col;
    char *map;
} lab_t;

int parseInput(const char *filename, lab_t *lab) {
    FILE *pFile;
    pFile = fopen(filename, "r");

    if (pFile == NULL) {
        perror("failed to open file");
        return 1;
    }

    lab->size = 0;

    int i = 0;
    char buffer[256];
    while (1) {
        fgets(buffer, 255, pFile);

        char *start = strchr(buffer, '^');
        if (start != NULL) {
            lab->start_row = i;
            lab->start_col = (int) (start - buffer);
        }
        
        if (feof(pFile)) break;

        if (lab->size == 0) {
            lab->size = strlen(buffer) - 1;
            lab->map = (char*) malloc(lab->size * lab->size);
        }

        memcpy(lab->map + (lab->size*i), buffer, lab->size);
        i++;
    }

    fclose(pFile);
    return 0;
}

#define D_UP    1
#define D_DOWN  2
#define D_LEFT  4
#define D_RIGHT 8

int cwDir(int dir) {
    switch (dir) {
    case D_UP: return D_RIGHT;
    case D_DOWN: return D_LEFT;
    case D_LEFT: return D_UP;
    case D_RIGHT: return D_DOWN;
    default:
        perror("invalid direction");
        return 0;
    }
}

// returns 1 if row, col is an obstacle
// and 2 if it's out of bounds
int isObstacle(lab_t *lab, int row, int col) {
    if (row < 0 || row >= lab->size || col < 0 || col >= lab->size) {
        return 2;
    } else {
        char c = lab->map[row * lab->size + col];
        if (c == '#') return 1;
        else return 0;
    }
}

void updateRowCol(int row, int col, int dir, int *nrow, int *ncol) {
    switch(dir) {
    case D_UP:    *nrow = row - 1; *ncol = col; return;
    case D_DOWN:  *nrow = row + 1; *ncol = col; return;
    case D_LEFT:  *nrow = row; *ncol = col - 1; return;
    case D_RIGHT: *nrow = row; *ncol = col + 1; return;
    default:
        perror("invalid direction");
        return;
    }
}

// simulates the guard navigating the lab and returns 1 if the guard gets stuck in a loop, otherwise returns 0.
// also stores the number of unique locations the guard enters in the parameter seen
int guardLoops(lab_t *lab, int *seen) {
    
    int work_map[lab->size * lab->size];
    memset(work_map, 0, sizeof(int) * lab->size * lab->size);

    int curr_row = lab->start_row;
    int curr_col = lab->start_col;
    int curr_dir = D_UP;

    int next_row;
    int next_col;
    
    *seen = 0;

    int curr_idx = curr_row * lab->size + curr_col;
    while ((work_map[curr_idx] & curr_dir) == 0) {
        // printf("r=%d\tc=%d\td=%d\n", curr_row, curr_col, curr_dir);
        
        // if this is the first time visiting a space, note that it's been seen
        if (work_map[curr_idx] == 0) { (*seen)++; }

        // mark that we've entered the current location moving in the current direction
        work_map[curr_idx] |= curr_dir;

        // try to continue moving in the current direction unless an obstacle or edge is hit
        updateRowCol(curr_row, curr_col, curr_dir, &next_row, &next_col);
        while (isObstacle(lab, next_row, next_col) == 1) {
            curr_dir = cwDir(curr_dir);
            updateRowCol(curr_row, curr_col, curr_dir, &next_row, &next_col);
        }

        if (isObstacle(lab, next_row, next_col) == 2) return 0;
        
        curr_row = next_row;
        curr_col = next_col;

        curr_idx = curr_row * lab->size + curr_col;
    }
    
    return 1;
}

int solutionPart1(lab_t *lab) {
    int seen;
    if (guardLoops(lab, &seen) == 1) {
        perror("part 1 guard doesn't get stuck in loop");
    }
    return seen;
}

int solutionPart2(lab_t *lab) {
    int seen;
    int count = 0;
    for (int obst_row = 0; obst_row < lab->size; obst_row++) {
        for (int obst_col = 0; obst_col < lab->size; obst_col++) {
            int obst_idx = obst_row * lab->size + obst_col;
            if (lab->map[obst_idx] == '.') {
                lab->map[obst_idx] = '#';
                if (guardLoops(lab, &seen)) {
                    count++;
                }
                lab->map[obst_idx] = '.';
            }
        }
    }
    return count;
}

int main(int argc, char **argv) {

    lab_t lab;
    if (parseInput("input.txt", &lab)) { return 1; }

    for (int i = 0; i < lab.size; i++) {
        for (int j = 0; j < lab.size; j++) {
            printf("%c", lab.map[i*lab.size + j]);
        }
        printf("\n");
    }

    int seen = solutionPart1(&lab);
    int count = solutionPart2(&lab);
    printf("part1 solution = %d\n", seen);
    printf("part2 solution = %d\n", count);
    
    free(lab.map);
}
