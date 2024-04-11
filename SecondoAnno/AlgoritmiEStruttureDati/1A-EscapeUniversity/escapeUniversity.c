#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1000*1000

typedef struct {
    int id;
    int val;
} valu;

void merge(valu arr[], int left, int mid, int right) {
    int i, j, k;
    int n1 = mid - left + 1;
    int n2 = right - mid;

    valu L[n1], R[n2];

    for (i = 0; i < n1; i++)
        L[i] = arr[left + i];
    for (j = 0; j < n2; j++)
        R[j] = arr[mid + 1 + j];

    i = 0;
    j = 0;
    k = left;

    while (i < n1 && j < n2) {
        if (L[i].val <= R[j].val) {
            arr[k] = L[i];
            i++;
        } else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }

    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }

    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
}

void mergeSort(valu arr[], int left, int right) {
    if (left < right) {
        int mid = left + (right - left) / 2;

        mergeSort(arr, left, mid);
        mergeSort(arr, mid + 1, right);

        merge(arr, left, mid, right);
    }
}

int binarySearch(valu arr[], int left, int right, int target) {
    if (right >= left) {
        int mid = left + (right - left) / 2;

        if (arr[mid].val == target) {
            return mid;
        }

        if (arr[mid].val > target) {
            return binarySearch(arr, left, mid - 1, target);
        }

        return binarySearch(arr, mid + 1, right, target);
    }

    return -1; // target not found
}

valu find(valu values[], size_t values_size, int target) {
    int index = binarySearch(values, 0, values_size - 1, target);

    if (index != -1) {
        return values[index];
    }

    valu null_val = {0, 0}; // create a null value
    return null_val;
}


void solve(valu *values, int values_size, int query, FILE *out_file) {
    valu p = find(values, values_size, query);

    if (p.val == query) {
        fprintf(out_file, "%d ", p.id);
    } else {
        fprintf(out_file, "NULL");
    }
}

void escape_university(FILE *in_file, FILE *out_file) {
    int N, Q;
    fscanf(in_file, "%d %d", &N, &Q);

    valu values[MAX_SIZE];
    int values_size = 0;

    for (int i=0; i<N; i++) {
        int id, s;
        fscanf(in_file, "%d %d", &id, &s);

        for (int j=0; j<s-1; j++) {
            int x; 
            fscanf(in_file, "%d", &x);

            values[values_size].id = id;
            values[values_size].val = x;
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

int main(void) {
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