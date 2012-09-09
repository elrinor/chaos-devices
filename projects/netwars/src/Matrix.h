#ifndef __MATRIX_H__
#define __MATRIX_H__

template<class T>
class Matrix {
private:
  T** elem;
  int x;
  int y;

  Matrix& operator= (const Matrix&);
  Matrix(const Matrix&);

public:
  T* operator[] (int index) {
    return elem[index];
  }

  T const* operator[] (int index) const {
    return elem[index];
  }

  int sizeX() const {
    return this->x;
  }

  int sizeY() const {
    return this->y;
  }

  void fill(const T& value) {
    for (int i = 0; i < this->x; i++)
      for (int j = 0; j < this->y; j++)
        elem[i][j] = value;
  }

  Matrix(int x, int y, T defaultValue = T()) {
    this->x = x;
    this->y = y;
    elem = new T*[x];
    for(int i = 0; i < x; i++)
      elem[i] = new T[y];
    fill(defaultValue);
  }

  ~Matrix() {
    for(int i = 0; i < x; i++)
      delete[] elem[i];
    delete[] elem;
  }
};



#endif