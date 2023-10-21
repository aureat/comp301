#include <stdlib.h>
#include <string.h>

typedef struct {
    int *data;
    int front;
    int rear;
    int size;
    int capacity;
} Queue;

Queue *queue_new(int capacity) {
    Queue *q = (Queue *)malloc(sizeof(Queue));
    q->data = (int *)malloc(capacity * sizeof(int));
    q->front = 0;
    q->rear = -1;
    q->size = 0;
    q->capacity = capacity;
    return q;
}

int queue_isempty(Queue *q) {
    return q->size == 0;
}

int queue_isfull(Queue *q) {
    return q->size == q->capacity;
}

int queue_size(Queue *q) {
    return q->size;
}

int queue_capacity(Queue *q) {
    return q->capacity;
}

int *queue_tolist(Queue *q) {
    if (queue_isempty(q)) {
        return NULL;
    }

    int *elements = (int *)malloc(q->size * sizeof(int));
    if (elements == NULL) {
        return NULL;
    }

    int index = q->front;
    for (int i = 0; i < q->size; i++) {
        elements[i] = q->data[index];
        index = (index + 1) % q->capacity;
    }

    return elements;
}

int queue_resize(Queue *q, int new_capacity) {
    if (new_capacity < q->size) {
        return -1;
    }

    int *new_data = (int *)malloc(new_capacity * sizeof(int));
    if (new_data == NULL) {
        return -1;
    }

    int index = q->front;
    for (int i = 0; i < q->size; i++) {
        new_data[i] = q->data[index];
        index = (index + 1) % q->capacity;
    }

    free(q->data);
    q->data = new_data;
    q->capacity = new_capacity;
    q->front = 0;
    q->rear = q->size - 1;

    return 0;
}


int queue_peek(Queue *q) {
    if (queue_isempty(q)) {
        return -1;
    }
    return q->data[q->front];
}

void queue_push(Queue *q, int item) {
    if (queue_isfull(q)) {
        queue_resize(q, q->capacity * 2);
    }
    q->rear = (q->rear + 1) % q->capacity;
    q->data[q->rear] = item;
    q->size++;
}

void queue_push_multi(Queue *q, int *items, int count) {
    for (int i = 0; i < count; i++) {
        queue_push(q, items[i]);
    }
}

int queue_pop(Queue *q) {
    if (queue_isempty(q)) {
        return -1;
    }
    int item = q->data[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->size--;
    return item;
}

int queue_pop_multi(Queue *q, int count) {
    int dequeued_count = 0;
    for (int i = 0; i < count; i++) {
        if (queue_isempty(q))
            break;
        dequeued_count++;
    }
    return dequeued_count;
}

void queue_free(Queue *q) {
    free(q->data);
    free(q);
}

Queue *queue_merge(Queue *q1, Queue *q2) {
    while (!queue_isempty(q2)) {
        queue_push(q1, queue_pop(q2));
    }
    return q1;
}
