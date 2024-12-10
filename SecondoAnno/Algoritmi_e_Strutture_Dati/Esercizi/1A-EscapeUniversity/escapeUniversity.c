#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1000*1000

typedef struct {
    int id;
    int val;
} valu;

void merge(valu *values, int start, int mid, int end) {
    int n1 = mid - start + 1;
    int n2 = end - mid;

    int i, j;
    valu *left = malloc(n1 * sizeof(valu));
    valu *right = malloc(n2 * sizeof(valu));

    for (i=0; i<n1; i++) 
        left[i] = values[start+i];
    for (j=0; j<n2; j++) 
        right[j] = values[mid+1+j];
    
    i = j = 0;

    for (int k=start; k<=end; k++) {
        if (i < n1) {
            if (j < n2) {
                values[k] = (left[i].val <= right[j].val) ? left[i++] : right[j++];
            } else {
                values[k] = left[i++];
            }
        } else {
            values[k] = right[j++];
        }
    }

    free(left);
    free(right);
}

void mergeSort(valu *values, int start, int end) {
    if (start < end) {
        int mid = start + (end-start) / 2;

        mergeSort(values, start, mid);
        mergeSort(values, mid+1, end);

        merge(values, start, mid, end);
    }
}

valu find(valu *values, int low, size_t values_size, int target) {
    if (low > values_size)
        return values[0];

    int mid = (low + values_size) / 2;
    if (values[mid].val == target)
        return values[mid];
    if (values[mid].val < target)
        return find(values, mid+1, values_size, target);
    if (values[mid].val > target)
        return find(values, low, mid-1, target);
}

void solve(valu *values, int values_size, int query, FILE *out_file) {
    valu p = find(values, 0, values_size, query);

    if (p.val == query) {
        fprintf(out_file, "%d ", p.id);
    } else {
        fprintf(out_file, "NULL ");
    }
}

void escape_university(FILE *in_file, FILE *out_file) {
    valu values[MAX_SIZE];
    int values_size = 0;
    
    int N, Q;
    fscanf(in_file, "%d %d", &N, &Q);

    for (int i=0; i<N; i++) {
        int id, s;
        fscanf(in_file, "%d %d", &id, &s);

        for (int j=0; j<s-1; j++) {
            int val; 
            fscanf(in_file, "%d", &val);

            values[values_size].id = id;
            values[values_size].val = val;
            values_size++;
        }
    }

    mergeSort(values, 0, values_size);

    for (int i=0; i<Q; i++) {
        int query;
        fscanf(in_file, "%d", &query);
        solve(values, values_size, query, out_file);
    }
}

int main() {
    // Apro i file di input e output
    FILE *in_file = fopen("input.txt", "r");
    FILE *out_file = fopen("output.txt", "w");

    if (in_file == NULL || out_file == NULL) {
        return -1;
    }   

    escape_university(in_file, out_file);

    // Chiudo i file di input e ourput
    if (fclose(in_file) != 0 || fclose(out_file) != 0) {
        return -2;
    }

    return 0;
}