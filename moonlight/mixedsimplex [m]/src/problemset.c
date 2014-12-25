#include <stdlib.h>
#include <assert.h>
#include "problemset.h"


void problemset_grow(ProblemSet* p) {
    p->size = p->size * 2 + 1;
    p->problems = (Problem**) realloc(p->problems, p->size * sizeof(Problem*));
}

ProblemSet* problemset_new(int size) {
    ProblemSet* result = (ProblemSet*) malloc(sizeof(ProblemSet));
    assert(size >= 0);
    result->size = size;
    result->count = 0;
    result->problems = (Problem**) malloc(size * sizeof(Problem*));
    return result;
}

void problemset_delete(ProblemSet* p) {
    assert(p != NULL);
    free(p->problems);
    free(p);
}

void problemset_pushback(ProblemSet* p, Problem* problem) {
    assert(p != NULL && problem != NULL);
    if(p->count == p->size)
        problemset_grow(p);
    p->problems[p->count++] = problem;
}

void problemset_addsorted(ProblemSet* p, Problem* problem) {
    int i, j;
    assert(p != NULL && problem != NULL);
    if(p->count == p->size)
        problemset_grow(p);
    for(i = 0; i < p->count; i++)
        if(p->problems[i]->effectiveness < problem->effectiveness)
            break;
    for(j = p->count; j > i; j--)
        p->problems[j] = p->problems[j - 1];
    p->problems[i] = problem;
    p->count++;
}

Problem* problemset_remove(ProblemSet* p, int index) {
    int i;
    Problem* result;

    assert(p != NULL && index >= 0 && index < p->count);

    result = p->problems[index];
    for(i = index; i < p->count - 1; i++)
        p->problems[i] = p->problems[i + 1];
    p->count--;
    return result;
}


