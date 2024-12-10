#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_R 20
#define MAX_ANGLE 360
#define QUEUE_SIZE (21 * 360)

// Definire una struttura per rappresentare i muri
typedef struct {
    char type; // 'C' per muro circolare, 'S' per muro dritto
    int r1, r2; // raggio per muri dritti
    int theta1, theta2; // angoli per muri circolari
    int theta; // angolo per muri dritti
} Wall;

bool isBlocked[21][360]; // Matrice per tracciare i muri

void initializeMatrix() {
    for (int i = 0; i <= MAX_R; i++) {
        for (int j = 0; j < MAX_ANGLE; j++) {
            isBlocked[i][j] = false;
        }
    }
}

void addWall(Wall wall) {
    if (wall.type == 'C') {
        int r = wall.r1;
        int theta1 = wall.theta1;
        int theta2 = wall.theta2;
        if (theta1 > theta2) {
            for (int theta = theta1; theta < MAX_ANGLE; theta++) {
                isBlocked[r][theta] = true;
            }
            for (int theta = 0; theta <= theta2; theta++) {
                isBlocked[r][theta] = true;
            }
        } else {
            for (int theta = theta1; theta <= theta2; theta++) {
                isBlocked[r][theta] = true;
            }
        }
    } else if (wall.type == 'S') {
        int r1 = wall.r1;
        int r2 = wall.r2;
        int theta = wall.theta;
        for (int r = r1; r <= r2; r++) {
            isBlocked[r][theta] = true;
        }
    }
}

bool bfs() {
    // BFS per trovare un percorso dal centro all'esterno
    bool visited[21][360] = {false};
    int queue[QUEUE_SIZE][2]; // Coda per BFS: memorizza (r, theta)
    int front = 0, rear = 0;

    // Inizializzare la BFS dal centro (r = 1)
    for (int theta = 0; theta < MAX_ANGLE; theta++) {
        if (!isBlocked[1][theta]) {
            queue[rear][0] = 1;
            queue[rear][1] = theta;
            rear++;
            visited[1][theta] = true;
        }
    }

    while (front < rear) {
        int r = queue[front][0];
        int theta = queue[front][1];
        front++;

        // Se siamo al raggio massimo, abbiamo trovato un'uscita
        if (r == MAX_R) {
            return true;
        }

        // Muoversi nelle quattro direzioni
        int dr[4] = {1, -1, 0, 0};
        int dtheta[4] = {0, 0, 1, -1};

        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int ntheta = (theta + dtheta[i] + MAX_ANGLE) % MAX_ANGLE;

            if (nr >= 1 && nr <= MAX_R && !visited[nr][ntheta] && !isBlocked[nr][ntheta]) {
                if (rear >= QUEUE_SIZE) {
                    fprintf(stderr, "Errore: coda BFS superata\n");
                    return false;
                }
                queue[rear][0] = nr;
                queue[rear][1] = ntheta;
                rear++;
                visited[nr][ntheta] = true;
            }
        }
    }

    return false;
}

int main() {
    FILE *input = fopen("input.txt", "r");
    FILE *output = fopen("output.txt", "w");

    if (input == NULL || output == NULL) {
        fprintf(stderr, "Errore nell'apertura dei file.\n");
        return 1;
    }

    int N;
    fscanf(input, "%d", &N);

    for (int i = 0; i < N; i++) {
        int W;
        fscanf(input, "%d", &W);

        initializeMatrix();

        for (int j = 0; j < W; j++) {
            Wall wall;
            fscanf(input, " %c", &wall.type);

            if (wall.type == 'C') {
                fscanf(input, "%d %d %d", &wall.r1, &wall.theta1, &wall.theta2);
                addWall(wall);
            } else if (wall.type == 'S') {
                fscanf(input, "%d %d %d", &wall.r1, &wall.r2, &wall.theta);
                addWall(wall);
            }
        }

        if (bfs()) {
            fprintf(output, "YES\n");
        } else {
            fprintf(output, "NO\n");
        }
    }

    fclose(input);
    fclose(output);

    return 0;
}
