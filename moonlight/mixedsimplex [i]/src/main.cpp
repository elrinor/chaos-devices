#include <iostream>
#include <cmath>
#include <iomanip>
#include <cfloat>
#include "Symplex.h"

using namespace std;

//сссрс

class Task {
private:
    static int globalIndex;

    void init(double eff, int k, int f, int i, int b) {
        this->eff = eff;
        this->k = k;
        this->f = f;
        this->i = i;
        this->b = b;
        this->n = f + i + b;
        this->maximum = 0;
        this->index = globalIndex++;
        this->a = new double[(this->n + 1) * (this->k + 1)];
        this->solVector = new double[n];
		
        this->maxValues = new double[n];
        this->minValues = new double[n];
		
        for(int i = 0; i < n; i++) {
            this->minValues[i] = 0;
            this->maxValues[i] = DBL_MAX;
        }

        for(int i = f + this->i; i < n; i++)
            this->maxValues[i] = 1;
    }

public:
    static int count;


    static void setGlobalIndex(int newIndex) {
        globalIndex = newIndex;
    }

    double eff;
    int index;

    int n;
    int k;
    int f, i, b;

    double* minValues;
    double* maxValues;
    
    double* a;
    double* solVector;

    double maximum;

    void addMin(int index, double value) {
        this->minValues[index] = max(this->minValues[index], value);
    }

    void addMax(int index, double value) {
        this->maxValues[index] = min(this->maxValues[index], value);
    }

    Task(double eff, int k, int f, int i, int b) {
        count++;
        init(eff, k, f, i, b);
    }

    Task(const Task& t) {
        count++;
        init(t.maximum, t.k, t.f, t.i, t.b);
        copy(t.a, t.a + (t.n + 1) * (t.k + 1), this->a);
        copy(t.minValues, t.minValues + t.n, this->minValues);
        copy(t.maxValues, t.maxValues + t.n, this->maxValues);
    }

    Task(istream& s) {
        count++;
        s >> this->f >> this->i >> this->b >> this->k;
        if(s.eof() || s.fail())
            return;
        init(0, k, f, i, b);
        for(int i = 0; i < n; i++)
            s >> a[i];
        for(int i = 1; i <= k; i++) 
            for(int j = 0; j <= n; j++) 
                s >> a[i * (n + 1) + j];
        a[n] = 0;
    }

    ~Task() {
        count--;
        delete this->a;
        delete this->solVector;
        delete this->maxValues;
        delete this->minValues;
    }

    void print(ostream& s) {
        s << "Task #" << this->index << endl;
        s << "Eff " << this->eff << endl;
        for(int i = 0; i < n; i++)
            s << " " << setw(6) << a[i];
        s << " -> max" << endl;
        for(int j = 1; j <= k; j++){
            for(int i = 0; i <= n; i++)
                s << " " << setw(6) << a[j * (n + 1) + i];
            s << endl;
        }
        for(int i = 0; i < n; i++)
            cout << "x" << i << " in [" << setw(6) << minValues[i] << ", " << setw(6) << maxValues[i] << "]" << endl;
    }

    bool solve() {
        double* newa = new double[(n + 1) * (k + 2 * n + 1)];
        double* p = newa;
        copy(a, a + (n + 1), newa);
        for(int i = 0; i < n; i++)
            newa[i] *= -1;
        p += n + 1;

        int newk = 0;

        for(int i = 1; i < k + 1; i++) {
            double minv = 0;
            for(int j = 0; j < n; j++) {
                double coeff = a[i * (n + 1) + j];
                if(coeff < 0)
                    minv += maxValues[j] * coeff;
                else 
                    minv += minValues[j] * coeff;
            }
            if(minv >= a[i * (n + 1) + n]) {
                cout << "No need to solve, inconsistent" << endl;
                delete newa;
                return false;
            }
        }

        for(int i = 1; i < k + 1; i++) {
            double maxv = 0;
            for(int j = 0; j < n; j++) {
                double coeff = a[i * (n + 1) + j];
                if(coeff > 0)
                    maxv += maxValues[j] * coeff;
                else 
                    maxv += minValues[j] * coeff;
            }
            if(maxv >= a[i * (n + 1) + n]) {
                copy(a + i * (n + 1), a + i * (n + 1) + n + 1, p);
                p += n + 1;
                newk++;
            }
        }

        for(int i = 0; i < n; i++) {
            fill(p, p + n + 1, 0);
            p[i] = 1;
            p[n] = maxValues[i];
            p += n + 1;
            newk++;

            fill(p, p + n + 1, 0);
            p[i] = -1;
            p[n] = -minValues[i];
            p += n + 1;
            newk++;
        }
        
        maximum = symplex(newk, n, newa, solVector);
        delete newa;
        return maximum != _BAD_REG && maximum != _BAD_FUNC;
    }
};

int Task::count = 0;

int Task::globalIndex;

class TaskNode {
private:
   TaskNode* next;
   Task* task;

   static int count;

   friend class TaskList;

public:
    Task* getTask() {
        return this->task;
    }

    TaskNode* getNext() {
        return this->next;
    }

    bool hasNext() {
        return getNext() != NULL;
    }

    TaskNode(Task* task): task(task), next(NULL) {count++;};

    ~TaskNode() {count--;}
};

int TaskNode::count = 0;

class TaskList {
private:
    TaskNode root;
    int _size;

public:
    TaskList(): root(NULL) {
        root.next = NULL;
        _size = 0;
    }

    int size() {
        return this->_size;
    }

    void destroy() {
        TaskNode* n = root.next;
        while(n != NULL) {
            TaskNode* next = n->getNext();
            delete n->getTask();
            delete n;
            n = next;
        }
    }

    void push(Task* t) {
        this->_size++;
        TaskNode* n = new TaskNode(t);
        n->next = this->root.next;
        this->root.next = n;
    }

    Task* pop() {
        this->_size--;
        Task* t = root.next->task;
        TaskNode* n = root.next->next;
        delete root.next;
        root.next = n;
        return t;
    }

    void insertSorted(Task* t) {
        this->_size++;
        TaskNode* tnode = new TaskNode(t);
        TaskNode* last = &root;
        while(true) {
            if(last->next == NULL || last->next->task->eff < t->eff) {
                tnode->next = last->next;
                last->next = tnode;
                break;
            }
            last = last->next;
        }
    }

    void deleteUnefficient(double eff, TaskList& to) {
        TaskNode* last = &root;
        while(true) {
            if(last->next == NULL)
                break;
            if(last->next->task->eff < eff) {
                cout << "Task #" << last->next->task->index << " deleted" << endl;
                this->_size--;
                to.push(last->next->task);
                TaskNode* n = last->next;
                last->next = n->next;
                delete n;
            } else {
                last = last->next;
            }
        }
    }
};

inline bool notInt(double d) {
    return (abs(d - floor(d)) > 0.0000001 && abs(d - ceil(d)) > 0.0000001);
}

inline bool notBool(double d) {
    return abs(d) > 0.0000001 && abs(d - 1) > 0.0000001;
}

int main() {
    while(true) {
        Task::setGlobalIndex(0);
        Task* best = NULL;
        TaskList tasks, deleted;
        tasks.push(new Task(cin));
        if(cin.fail() || cin.eof())
            return 0;

        while(tasks.size() > 0) {
            Task* t = tasks.pop();
            t->print(cout);
            if(t->solve()) {
                cout << "Solved" << endl << "Solution = ( ";
                for(int i = 0; i < t->n; i++)
                    cout << " " << setw(6) << t->solVector[i];
                cout << ")" << endl << "Maximum = " << t->maximum << endl;

                if(best != NULL && best->maximum >= t->maximum) {
                    cout << "Solution is worse than the best one" << endl;
                    deleted.push(t);
                } else {
                    int ni = -1;
                    for(int i = t->f; i < t->f + t->i; i++) {
                        if(notInt(t->solVector[i])) {
                            ni = i;
                            break;
                        }
                    }

                    int nb = -1;
                    if(ni == -1) {
                        for(int i = t->f + t->i; i < t->n; i++) {
                            if(notBool(t->solVector[i])) {
                                nb = i;
                                break;
                            }
                        }
                    }


                    if(nb != -1) {
                        Task *ta, *at;

                        ta = new Task(*t);
                        at = new Task(*t);

                        ta->addMin(nb, 1);
                        at->addMax(nb, 0);
/*
                        ta = new Task(t->maximum, t->k + 2, t->f, t->i, t->b);
                        at = new Task(t->maximum, t->k + 1, t->f, t->i, t->b);
                        copy(t->a, t->a + (t->n + 1) * (t->k + 1), ta->a);
                        copy(t->a, t->a + (t->n + 1) * (t->k + 1), at->a);

                        for(int i = 0; i < ta->n + 1; i++) {
                            ta->a[(t->k + 1) * (ta->n + 1) + i] = 0;
                            ta->a[(t->k + 2) * (ta->n + 1) + i] = 0;
                            at->a[(t->k + 1) * (at->n + 1) + i] = 0;
                        }

                        ta->a[(t->k + 1) * (ta->n + 1) + nb]    = 1;
                        ta->a[(t->k + 1) * (ta->n + 1) + ta->n] = 1;
                        ta->a[(t->k + 2) * (ta->n + 1) + nb]    = -1;
                        ta->a[(t->k + 2) * (ta->n + 1) + ta->n] = -1;

                        at->a[(t->k + 1) * (at->n + 1) + nb] = 1;
*/
                        cout << "Non-boolean solution" << endl;
                        cout << "Created tasks #" << ta->index << " and #" << at->index << endl;

                        tasks.insertSorted(ta);
                        tasks.insertSorted(at);

                        deleted.push(t);
                    } else if(ni != -1) {
                        Task *ta, *at;

                        ta = new Task(*t);
                        at = new Task(*t);

                        ta->addMin(ni, ceil(t->solVector[ni]));
                        at->addMax(ni, floor(t->solVector[ni]));

/*
                        ta = new Task(t->maximum, t->k + 1, t->f, t->i, t->b);
                        at = new Task(t->maximum, t->k + 1, t->f, t->i, t->b);
                        copy(t->a, t->a + (t->n + 1) * (t->k + 1), ta->a);
                        copy(t->a, t->a + (t->n + 1) * (t->k + 1), at->a);

                        for(int i = 0; i < ta->n + 1; i++) {
                            ta->a[(t->k + 1) * (ta->n + 1) + i] = 0;
                            at->a[(t->k + 1) * (at->n + 1) + i] = 0;
                        }

                        ta->a[(t->k + 1) * (ta->n + 1) + ni]    = 1;
                        ta->a[(t->k + 1) * (ta->n + 1) + ta->n] = floor(t->solVector[ni]);
                        at->a[(t->k + 1) * (at->n + 1) + ni]    = -1;
                        at->a[(t->k + 1) * (at->n + 1) + at->n] = -ceil(t->solVector[ni]);
*/
                        cout << "Non-integer solution" << endl;
                        cout << "Created tasks #" << ta->index << " and #" << at->index << endl;

                        tasks.insertSorted(ta);
                        tasks.insertSorted(at);

                        deleted.push(t);
                    } else {
                        cout << "Best solution" << endl;
                        if(best != NULL)
                            deleted.push(best);
                        best = t;
                        if(t->index > 20)
                            t->eff++;
                        tasks.deleteUnefficient(t->maximum, deleted);
                    }
                }
            } else {
                cout << "Failed to solve" << endl;
                deleted.push(t);
            }

            cout << endl << endl;
        }

        if(best == NULL) {
            cout << "No solution found" << endl;
        } else {
            cout << "Solution found:" << endl;
            cout << "Solution = (";
            for(int i = 0; i < best->n; i++)
                cout << " " << setw(6) << best->solVector[i];
            cout << " )" << endl << "Maximum = " << best->maximum << endl;
            delete best;
        }
        cout << "TASKS CREATED: " << Task::count << endl << endl << endl << endl << endl << endl << endl << endl << endl;

        deleted.destroy();
    }

    return 0;
}