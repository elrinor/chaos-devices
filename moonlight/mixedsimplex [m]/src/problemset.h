#ifndef __PROBLEMSET_H__
#define __PROBLEMSET_H__

#include "problem.h"

typedef struct _ProblemSet {
    Problem** problems;
    int size;
    int count;
} ProblemSet;

ProblemSet* problemset_new(int size);

void problemset_delete(ProblemSet* p);

void problemset_pushback(ProblemSet* p, Problem* problem);

Problem* problemset_remove(ProblemSet* p, int index);

void problemset_addsorted(ProblemSet* p, Problem* problem);

#endif