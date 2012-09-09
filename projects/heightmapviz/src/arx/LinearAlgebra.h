#ifndef __ARX_LINEARALGEBRA_H__
#define __ARX_LINEARALGEBRA_H__

#include "config.h"

#ifdef ARX_USE_EIGEN

#include <Eigen/Core>
#include <Eigen/Array>
#include <Eigen/Geometry>

namespace arx {
  using namespace Eigen;
}

#else // ARX_USE_EIGEN

#include <cassert>
#include <iostream>
#include <algorithm>
#include <limits>
#include "smart_ptr.h"
#include "static_assert.h"
#include "Preprocessor.h"
#include "Mpl.h"
#include "TypeTraits.h"
#include "Utility.h"
#include "Memory.h"

#ifdef min
#  undef min
#  define MIN_UNDEFFED
#endif
#ifdef max
#  undef max
#  define MAX_UNDEFFED
#endif

#ifdef ARX_MSVC
#  /* We want hardcore inlining */
#  pragma inline_recursion(on)
#  pragma inline_depth(255)
#endif

namespace arx {
  /**
   * This number indicates that the size of the vector or matrix is determined at compile-time.
   * It must be big enough, yet not too big to cause arithmetic overflows.
   */
  enum { DYNAMIC_SIZE = 40000 };

// -------------------------------------------------------------------------- //
// Macro helpers
// -------------------------------------------------------------------------- //
#define ARX_INHERIT_ASSIGNMENT_OPERATOR(FROMCLASS, OP)                          \
  template<class OtherDerived>                                                  \
  this_type& operator OP (const MatrixBase<OtherDerived>& that) {               \
    return static_cast<this_type&>(FROMCLASS::operator OP(that));               \
  }                                                                             \
  this_type& operator OP (const this_type& that) {                              \
    return static_cast<this_type&>(FROMCLASS::operator OP(that));               \
  }                                                                             \

#define ARX_INHERIT_SCALAR_ASSIGNMENT_OPERATOR(FROMCLASS, OP)                   \
  template<class OtherT>                                                        \
  this_type& operator OP(const OtherT& scalar) {                                \
    return static_cast<this_type&>(FROMCLASS::operator OP(scalar));             \
  }                                                                             \

#define ARX_INHERIT_ASSIGNMENT_OPERATORS_FROM(FROMCLASS)                        \
  ARX_INHERIT_ASSIGNMENT_OPERATOR(FROMCLASS, +=)                                \
  ARX_INHERIT_ASSIGNMENT_OPERATOR(FROMCLASS, -=)                                \
  ARX_INHERIT_ASSIGNMENT_OPERATOR(FROMCLASS, =)                                 \
  ARX_INHERIT_SCALAR_ASSIGNMENT_OPERATOR(FROMCLASS, *=)                         \
  ARX_INHERIT_SCALAR_ASSIGNMENT_OPERATOR(FROMCLASS, /=)                         \

#define ARX_INHERIT_ASSIGNMENT_OPERATORS()                                      \
  ARX_INHERIT_ASSIGNMENT_OPERATORS_FROM(MatrixBase)                             \

#define ARX_BEFRIEND_MATRIXBASE_WPREFIX(PREFIX)                                 \
  friend class PREFIX MatrixBase<this_type>;                                    \

#define ARX_BEFRIEND_MATRIXBASE()                                               \
  ARX_BEFRIEND_MATRIXBASE_WPREFIX(ARX_EMPTY())                                  \

#define ARX_DYNAMIC_NUMBERS_EQ(A, B)                                            \
  ((A) == DYNAMIC_SIZE || (B) == DYNAMIC_SIZE || (A) == (B))

#define ARX_SAME_MATRIX_SIZE(L, R)                                              \
  (ARX_DYNAMIC_NUMBERS_EQ(Traits<L>::ColsAtCompileTime, Traits<R>::ColsAtCompileTime) && \
   ARX_DYNAMIC_NUMBERS_EQ(Traits<L>::RowsAtCompileTime, Traits<R>::RowsAtCompileTime))

#define ARX_SAME_VECTOR_SIZE(L, R)                                              \
  ARX_DYNAMIC_NUMBERS_EQ(Traits<L>::SizeAtCompileTime, Traits<R>::SizeAtCompileTime)

#define ARX_VECTOR_ONLY(M)                                                      \
  (Traits<M>::IsVectorAtCompileTime)

#define ARX_STATIC_ASSERT_DYNAMIC_NUMBERS_EQ(A, B)                              \
  STATIC_ASSERT((ARX_DYNAMIC_NUMBERS_EQ(A, B)))

#define ARX_STATIC_ASSERT_SAME_MATRIX_SIZE(L, R)                                \
  STATIC_ASSERT((ARX_SAME_MATRIX_SIZE(L, R)))

#define ARX_STATIC_ASSERT_SAME_VECTOR_SIZE(L, R)                                \
  STATIC_ASSERT((ARX_SAME_VECTOR_SIZE(L, R)))

#define ARX_STATIC_ASSERT_VECTOR_ONLY(M)                                        \
  STATIC_ASSERT((ARX_VECTOR_ONLY(M)))

#define ARX_STATIC_ASSERT_SAME_VALUETYPE(L, R)                                  \
  STATIC_ASSERT((is_same<typename Traits<L>::value_type, typename Traits<R>::value_type>::value))

  /** This definition determines the alignment for all Matrix types. It is needed for SSE vectorization 
   * (which is not yet implemented) */
#define ARX_VEC_ALIGNMENT 16

#define ARX_VEC_ALIGNED ALIGN(ARX_VEC_ALIGNMENT)

#define ARX_VEC_ALIGNED_MALLOC(size) aligned_malloc((size), ARX_VEC_ALIGNMENT)

#define ARX_VEC_ALIGNED_FREE(ptr) aligned_free(ptr)

  /*
   * Derived:
   *   T& coeff(r, c);
   *   const T coeff(r, c) const;
   *   T& coeff(index);
   *   const T coeff(index) const;
   *   int rows() const;
   *   int cols() const;
   */

// -------------------------------------------------------------------------- //
// Forwards
// -------------------------------------------------------------------------- //
  template<class Derived> class MatrixBase;
  template<class T, int R, int C, int StorageOrder> class Matrix;
  template<class L, class R, class O> class CwiseBinaryOp;
  template<class M, class O> class CwiseUnaryOp;
  template<class T, int R, int C, class O> class CwiseNullaryOp;
  template<class L, class R> class MatrixMul;
  template<class M> class Transpose;
  template<class T, int R, int C> class Identity;
  template<class M, int R, int C> class Block;


// -------------------------------------------------------------------------- //
// MatrixFlags
// -------------------------------------------------------------------------- //
  enum MatrixFlags {
    /** Store matrix in a row-major order. In case we're working on an expression, this flag
    * determines the storage order of the matrix created by evaluation of expression. */
    RowMajorBit = 0x1,

    /** This flag determines whether the expression should be evaluated and stored into 
    * temporary before nesting. */
    EvalBeforeNestingBit = 0x2,

    /** This flag determines whether the expression should be evaluated and stored into 
    * temporary before assignment. Used to solve aliasing problems in matrix product. */
    EvalBeforeAssigningBit = 0x4,

    /** This flag determines whether the expression can be seen as 1D vector, i.e. whether it 
    * supports coeff(int) method without any overhead. */
    LinearAccessBit = 0x10,

    /** This flag determines whether the first coefficient of matrix is aligned. */
    AlignedBit = 0x40,

    /** This flag means the expression's elements could be accessed only once. Therefore,
    * if one needs to access them several times, like in matrix product, he should
    * evaluate it into temporary first. */
    NonRepeatableBit = 0x10000000
  };

  enum {
    RowMajor = RowMajorBit,
    ColMajor = 0
  };


// -------------------------------------------------------------------------- //
// Utility classes
// -------------------------------------------------------------------------- //
  template<int value>
  class int_if_dynamic {
  public:
    int getValue() const { return value; }
    void setValue(int v) {}
    int_if_dynamic(int v) {}
  };

  template<>
  class int_if_dynamic<DYNAMIC_SIZE> {
  private:
    int v;
  public:
    int getValue() const { return this->v; }
    void setValue(int v) { this->v = v; }
    int_if_dynamic(int v): v(v) {}
  };


// -------------------------------------------------------------------------- //
// NumTraits
// -------------------------------------------------------------------------- //
  template<typename T> struct NumTraits;

  template<> struct NumTraits<int> {
    typedef double FloatingPoint;
    enum {
      HasFloatingPoint = 0,
      ReadCost = 1,
      AddCost = 1,
      MulCost = 1
    };
  };

  template<> struct NumTraits<float> {
    typedef float FloatingPoint;
    enum {
      HasFloatingPoint = 1,
      ReadCost = 1,
      AddCost = 1,
      MulCost = 1
    };
  };

  template<> struct NumTraits<double> {
    typedef double FloatingPoint;
    enum {
      HasFloatingPoint = 1,
      ReadCost = 1,
      AddCost = 1,
      MulCost = 1
    };
  };


// -------------------------------------------------------------------------- //
// Functors
// -------------------------------------------------------------------------- //
  template<class T>
  struct Add {
    const T operator()(const T& a, const T& b) const {
      return a + b;
    }
    enum {
      Cost = NumTraits<T>::AddCost,
      Flags = 0
    };
  };

  template<class T>
  struct Sub {
    const T operator()(const T& a, const T& b) const {
      return a - b;
    }
    enum {
      Cost = NumTraits<T>::AddCost,
      Flags = 0
    };
  };

  template<class T>
  struct Mul {
    const T operator()(const T& a, const T& b) const {
      return a * b;
    }
    enum {
      Cost = NumTraits<T>::MulCost,
      Flags = 0
    };
  };

  template<class T>
  struct Div {
    const T operator()(const T& a, const T& b) const {
      return a / b;
    }
    enum {
      Cost = NumTraits<T>::MulCost,
      Flags = 0
    };
  };

  template<class T>
  struct Neg {
    const T operator()(const T& a) const {
      return -a;
    }
    enum {
      Cost = NumTraits<T>::AddCost,
      Flags = 0
    };
  };

  template<class T>
  struct Peq {
    const T operator()(const T& a) const {
      return +a;
    }
    enum {
      Cost = 0,
      Flags = 0
    };
  };

  template<class T>
  struct MulC {
    MulC(T scalar): scalar(scalar) {}
    const T operator()(const T& a) const {
      return a * this->scalar;
    }
    enum {
      Cost = NumTraits<T>::MulCost,
      Flags = 0
    };
  private:
    T scalar;
  };

  template<class T>
  struct PreMulC {
    PreMulC(T scalar): scalar(scalar) {}
    const T operator()(const T& a) const {
      return this->scalar * a;
    }
    enum {
      Cost = NumTraits<T>::MulCost,
      Flags = 0
    };
  private:
    T scalar;
  };

  template<class T>
  struct DivC {
    DivC(T scalar): scalar(scalar) {}
    const T operator()(const T& a) const {
      return a / this->scalar;
    }
    enum {
      Cost = NumTraits<T>::MulCost,
      Flags = 0
    };
  private:
    T scalar;
  };

  template<class T>
  struct Random {
    Random() {}
    const T operator()() const {
      return (NumTraits<T>::HasFloatingPoint ? 10 /* TODO */ : 1) * (rand() / (RAND_MAX + 1.0f));
    }
    enum {
      Cost = NumTraits<T>::MulCost * 5,
      Flags = NonRepeatableBit
    };
  };

  template<class T>
  struct Constant {
    Constant(T scalar): scalar(scalar) {}
    const T operator()() const {
      return this->scalar;
    }
    enum {
      Cost = 0,
      Flags = 0
    };
  private:
    T scalar;
  };

  template<class T>
  struct Assign {
    void operator()(T& l, const T& r) const {
      l = r;
    }
    enum {
      Cost = 0,
      Flags = 0
    };
  };

  template<class T>
  struct Min {
    Min() {}
    const T operator()(const T& l, const T& r) const {
      return std::min(l, r);
    }
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T>
  struct Max {
    Max() {}
    const T operator()(const T& l, const T& r) const {
      return std::max(l, r);
    }
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T>
  struct Abs {
    Abs() {}
    const T operator()(const T& a) const {
      return std::abs(a);
    }
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T>
  struct Dot {
    Dot(): scalar(static_cast<T>(0)) {}
    const T get() { return this->scalar; }
    void operator()(const T& l, const T& r) {
      this->scalar += l * r;
    }
    enum {
      Cost = NumTraits<T>::AddCost + NumTraits<T>::MulCost,
      Flags = 0
    };
  private:
    T scalar;
  };


// -------------------------------------------------------------------------- //
// FunctorTraits
// -------------------------------------------------------------------------- //
  template<class T> struct FunctorTraits {
    enum {
      Cost = T::Cost,
      Flags = T::Flags
    };
  };

  template<class T> struct FunctorTraits<std::less<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T> struct FunctorTraits<std::less_equal<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T> struct FunctorTraits<std::greater<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T> struct FunctorTraits<std::greater_equal<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T> struct FunctorTraits<std::equal_to<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };

  template<class T> struct FunctorTraits<std::not_equal_to<T> > {
    enum {
      Cost = NumTraits<T>::AddCost, /* This one isn't that obvious. */
      Flags = 0
    };
  };


// -------------------------------------------------------------------------- //
// Traits
// -------------------------------------------------------------------------- //
  template<class M> struct Traits;

  /**
   * Default implementation returns TraitsBase for corresponding MatrixBase class, i.e. ascends the hierarchy.
   * This was done to handle descendants of MatrixBase-derived classes.
   */
  template<class M> struct TraitsBase: public TraitsBase<typename M::base_type> {};


  /**
   * Implementation of nesting of different expression classes into other expressions
   * while paying attention to the need of evaluation.
   *
   * @param M matrix type which is being nested
   * @param elemAccessCount number of times each element of M will be accessed when evaluating enclosing
   *   expression, which M is being nested into
   * @param PlainMatrixType type to use in case it was decided to evaluate M before nesting
   */
  template<class M, int elemAccessCount = 1, class PlainMatrixType = typename Traits<M>::EvalType> struct Nested {
    enum {
      EvalCost   = 
        (elemAccessCount + 1) * NumTraits<typename Traits<M>::value_type>::ReadCost + 
        Traits<M>::CoeffReadCost,
      NoEvalCost = 
        elemAccessCount * Traits<M>::CoeffReadCost,

      PerformEval = (Traits<M>::Flags & EvalBeforeNestingBit) || 
        ((Traits<M>::Flags & NonRepeatableBit) && elemAccessCount > 1) || 
        (EvalCost < NoEvalCost)
    };
    typedef 
      typename if_c<PerformEval, PlainMatrixType, const M&>::type type;
  };


  /**
   * Flag inheritance implementation for nesting related flags.
   *
   * @see class Nested
   */
  template<class M, int elemAccessCount = 1>
  struct InheritNestingFlags {
    enum {
      value = (TraitsBase<M>::Flags & RowMajorBit) | 
        (Nested<M, elemAccessCount>::PerformEval ? 0 : 
         (TraitsBase<M>::Flags & (EvalBeforeAssigningBit | NonRepeatableBit)))
    };
  };


  /**
   * Implementation of TraitsBase for MatrixBase returns TraitsBase for corresponding Derived class, 
   * i.e. descends the hierarchy.
   */
  template<class Derived> struct TraitsBase<MatrixBase<Derived> >: public TraitsBase<Derived> {};


  /**
   * TraitsBase for Matrix.
   */
  template<class T, int R, int C, int storageOrder> struct TraitsBase<Matrix<T, R, C, storageOrder> > {
    STATIC_ASSERT((storageOrder == RowMajor || storageOrder == ColMajor));
    STATIC_ASSERT((R > 0 && C > 0));

    typedef T value_type;
    enum {
      RowsAtCompileTime = R,
      ColsAtCompileTime = C,
      StorageOrder = storageOrder,
      CoeffReadCost = NumTraits<value_type>::ReadCost,
      Flags = (StorageOrder == RowMajor ? RowMajorBit : 0) | AlignedBit | LinearAccessBit
    };

    typedef Matrix<T, R, C, StorageOrder> PlainMatrixType;
    typedef const PlainMatrixType& EvalType;
  };


  /**
   * TraitsBase for CwiseBinaryOp.
   */
  template<class L, class R, class O> struct TraitsBase<CwiseBinaryOp<L, R, O> > {
    ARX_STATIC_ASSERT_SAME_VALUETYPE(L, R);
    ARX_STATIC_ASSERT_SAME_MATRIX_SIZE(L, R);

    typedef typename TraitsBase<L>::value_type value_type;
    enum {
      RowsAtCompileTime = Traits<L>::HasDynamicRows ? Traits<R>::RowsAtCompileTime : Traits<L>::RowsAtCompileTime,
      ColsAtCompileTime = Traits<L>::HasDynamicCols ? Traits<R>::ColsAtCompileTime : Traits<L>::ColsAtCompileTime,
      CoeffReadCost = TraitsBase<L>::CoeffReadCost + TraitsBase<R>::CoeffReadCost + FunctorTraits<O>::Cost,
      Flags = (InheritNestingFlags<L>::value | InheritNestingFlags<R>::value) | 
        (Traits<L>::Flags & Traits<R>::Flags & (LinearAccessBit | AlignedBit)) |
        FunctorTraits<O>::Flags,
      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };
    typedef typename Nested<L>::type LNested;
    typedef typename Nested<R>::type RNested;

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * TraitsBase for CwiseUnaryOp.
   */
  template<class M, class O> struct TraitsBase<CwiseUnaryOp<M, O> > {
    typedef typename TraitsBase<M>::value_type value_type;
    enum {
      RowsAtCompileTime = Traits<M>::RowsAtCompileTime,
      ColsAtCompileTime = Traits<M>::ColsAtCompileTime,
      CoeffReadCost = TraitsBase<M>::CoeffReadCost + FunctorTraits<O>::Cost,
      Flags = InheritNestingFlags<M>::value | 
        (Traits<M>::Flags & (LinearAccessBit | AlignedBit)) |
        FunctorTraits<O>::Flags,
      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };
    typedef typename Nested<M>::type MNested;

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * TraitsBase for CwiseNullaryOp.
   */
  template<class T, int R, int C, class O> struct TraitsBase<CwiseNullaryOp<T, R, C, O> > {
    typedef T value_type;
    enum {
      RowsAtCompileTime = R,
      ColsAtCompileTime = C,
      CoeffReadCost = FunctorTraits<O>::Cost,
      Flags = LinearAccessBit | AlignedBit | FunctorTraits<O>::Flags,
      StorageOrder = ColMajor
    };

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * TraitsBase for MatrixMul.
   */
  template<class L, class R> struct TraitsBase<MatrixMul<L, R> > {
    ARX_STATIC_ASSERT_SAME_VALUETYPE(L, R);
    ARX_STATIC_ASSERT_DYNAMIC_NUMBERS_EQ(Traits<L>::ColsAtCompileTime, Traits<R>::RowsAtCompileTime);
    
    typedef typename TraitsBase<L>::value_type value_type;
    enum {
      RowsAtCompileTime = Traits<L>::RowsAtCompileTime,
      ColsAtCompileTime = Traits<R>::ColsAtCompileTime,

      InnerSize = Traits<L>::ColsAtCompileTime, /**< Size of the inner loop */
      CoeffReadCost = InnerSize == DYNAMIC_SIZE ? DYNAMIC_SIZE :
        InnerSize * (NumTraits<value_type>::MulCost + TraitsBase<L>::CoeffReadCost + TraitsBase<R>::CoeffReadCost) +
        (InnerSize - 1) * NumTraits<value_type>::AddCost,

      Flags = 
        (InheritNestingFlags<L, ColsAtCompileTime>::value | InheritNestingFlags<R, RowsAtCompileTime>::value) |
        (Traits<L>::Flags & Traits<R>::Flags & AlignedBit) | /* We don't have LinearAccessBit here */
        EvalBeforeAssigningBit,

      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };
    typedef typename Nested<L, ColsAtCompileTime>::type LNested;
    typedef typename Nested<R, RowsAtCompileTime>::type RNested;

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * TraitsBase for Transpose.
   */
  template<class M> struct TraitsBase<Transpose<M> > {
    typedef typename TraitsBase<M>::value_type value_type;
    enum {
      RowsAtCompileTime = Traits<M>::ColsAtCompileTime,
      ColsAtCompileTime = Traits<M>::RowsAtCompileTime,
      CoeffReadCost = TraitsBase<M>::CoeffReadCost,
      Flags = (InheritNestingFlags<M>::value ^ RowMajorBit) | 
        (Traits<M>::Flags & (LinearAccessBit | AlignedBit)),

      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };
    typedef typename Nested<M>::type MNested;

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * TraitsBase for Identity.
   */
  template<class T, int R, int C> struct TraitsBase<Identity<T, R, C> > {
    STATIC_ASSERT((R > 0 && C > 0));
    
    typedef T value_type;
    enum {
      RowsAtCompileTime = R,
      ColsAtCompileTime = C,
      CoeffReadCost = 1, /* "if" is a costy operation... */
      Flags = 0,
      StorageOrder = ColMajor
    };

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * Traits for Block
   */
  template<class M, int R, int C> struct TraitsBase<Block<M, R, C> > {
    STATIC_ASSERT((R > 0 && C > 0));
    
    typedef typename TraitsBase<M>::value_type value_type;
    enum {
      RowsAtCompileTime = R,
      ColsAtCompileTime = C,
      CoeffReadCost = 1, 
      Flags = (InheritNestingFlags<M>::value) | 
        (Traits<M>::IsVectorAtCompileTime ? LinearAccessBit : LinearAccessBit),
      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };
    typedef typename Nested<M>::type MNested;

    typedef Matrix<value_type, RowsAtCompileTime, ColsAtCompileTime, StorageOrder> PlainMatrixType;
    typedef PlainMatrixType EvalType;
  };


  /**
   * Traits class defines several traits of the classes derived from MatrixBase.
   */
  template<class M> struct Traits: public TraitsBase<M> {
    enum {
      HasDynamicRows = (RowsAtCompileTime == DYNAMIC_SIZE), 
      HasDynamicCols = (ColsAtCompileTime == DYNAMIC_SIZE),
      HasStaticRows = !HasDynamicRows, 
      HasStaticCols = !HasDynamicCols,
      IsVectorAtCompileTime = (RowsAtCompileTime == 1 || ColsAtCompileTime == 1),
      IsMatrixAtCompileTime = !IsVectorAtCompileTime,
      IsStaticallySized = HasStaticRows && HasStaticCols,
      IsDynamicallySized = !IsStaticallySized,
      IsStackAllocated = IsStaticallySized,
      IsHeapAllocated = IsDynamicallySized, 
      SizeAtCompileTime = (HasStaticRows && HasStaticCols) ? (RowsAtCompileTime * ColsAtCompileTime) : DYNAMIC_SIZE,
    };
  };


// -------------------------------------------------------------------------- //
// Inverter
// -------------------------------------------------------------------------- //
  template<int R> struct Inverter {
    /* Not implemented */
  };

  template<> struct Inverter<1> {
    template<class SrcDerived, class DstDerived>
    void operator()(const MatrixBase<SrcDerived>& src, MatrixBase<DstDerived>& dst) {
      typedef Traits<SrcDerived>::value_type value_type;

      typename Nested<SrcDerived>::type src(psrc.derived());

      dst(0, 0) = value_type(1) / src(0, 0);
    }
  };

  template<> struct Inverter<2> {
    template<class SrcDerived, class DstDerived>
    void operator()(const MatrixBase<SrcDerived>& psrc, MatrixBase<DstDerived>& dst) {
      typedef Traits<SrcDerived>::value_type value_type;
      
      typename Nested<SrcDerived, 2>::type src(psrc.derived());

      const value_type invdet = value_type(1) / (src(0, 0) * src(1, 1) - src(0, 1) * src(1, 0));
      dst(0,0) =  src(1,1) * invdet;
      dst(1,0) = -src(1,0) * invdet;
      dst(0,1) = -src(0,1) * invdet;
      dst(1,1) =  src(0,0) * invdet;
    }
  };

  template<> struct Inverter<3> {
    template<class SrcDerived, class DstDerived>
    void operator()(const MatrixBase<SrcDerived>& psrc, MatrixBase<DstDerived>& dst) {
      typedef Traits<SrcDerived>::value_type value_type;

      typename Nested<SrcDerived, 4>::type src(psrc.derived());

      const value_type det_minor00 = src(1, 1) * src(2, 2) - src(1, 2) * src(2, 1);
      const value_type det_minor10 = src(0, 1) * src(2, 2) - src(0, 2) * src(2, 1);
      const value_type det_minor20 = src(0, 1) * src(1, 2) - src(0, 2) * src(1, 1);
      const value_type det_minor01 = src(1, 0) * src(2, 2) - src(1, 2) * src(2, 0);
      const value_type det_minor11 = src(0, 0) * src(2, 2) - src(0, 2) * src(2, 0);
      const value_type det_minor21 = src(0, 0) * src(1, 2) - src(0, 2) * src(1, 0);
      const value_type det_minor02 = src(1, 0) * src(2, 1) - src(1, 1) * src(2, 0);
      const value_type det_minor12 = src(0, 0) * src(2, 1) - src(0, 1) * src(2, 0);
      const value_type det_minor22 = src(0, 0) * src(1, 1) - src(0, 1) * src(1, 0);

      const value_type det = det_minor00 * src(0, 0) - det_minor10 * src(1, 0) + det_minor20 * src(2, 0);
      const value_type invdet = value_type(1) / det;
      dst(0, 0) =  det_minor00 * invdet;
      dst(0, 1) = -det_minor10 * invdet;
      dst(0, 2) =  det_minor20 * invdet;
      dst(1, 0) = -det_minor01 * invdet;
      dst(1, 1) =  det_minor11 * invdet;
      dst(1, 2) = -det_minor21 * invdet;
      dst(2, 0) =  det_minor02 * invdet;
      dst(2, 1) = -det_minor12 * invdet;
      dst(2, 2) =  det_minor22 * invdet;
    }
  };

  template<> struct Inverter<4> {
    template<class SrcDerived, class DstDerived>
    void operator()(const MatrixBase<SrcDerived>& psrc, MatrixBase<DstDerived>& dst) {
      typedef Traits<SrcDerived>::value_type value_type;

      typename Nested<SrcDerived, 6>::type src(psrc.derived());

      const value_type a0 = src(0, 0) * src(1, 1) - src(0, 1) * src(1, 0);
      const value_type a1 = src(0, 0) * src(1, 2) - src(0, 2) * src(1, 0);
      const value_type a2 = src(0, 0) * src(1, 3) - src(0, 3) * src(1, 0);
      const value_type a3 = src(0, 1) * src(1, 2) - src(0, 2) * src(1, 1);
      const value_type a4 = src(0, 1) * src(1, 3) - src(0, 3) * src(1, 1);
      const value_type a5 = src(0, 2) * src(1, 3) - src(0, 3) * src(1, 2);
      const value_type b0 = src(2, 0) * src(3, 1) - src(2, 1) * src(3, 0);
      const value_type b1 = src(2, 0) * src(3, 2) - src(2, 2) * src(3, 0);
      const value_type b2 = src(2, 0) * src(3, 3) - src(2, 3) * src(3, 0);
      const value_type b3 = src(2, 1) * src(3, 2) - src(2, 2) * src(3, 1);
      const value_type b4 = src(2, 1) * src(3, 3) - src(2, 3) * src(3, 1);
      const value_type b5 = src(2, 2) * src(3, 3) - src(2, 3) * src(3, 2);

      const value_type invdet = value_type(1) / (a0 * b5 - a1 * b4 + a2 * b3 + a3 * b2 - a4 * b1 + a5 * b0);

      dst(0, 0) = ( + src(1, 1) * b5 - src(1, 2) * b4 + src(1, 3) * b3) * invdet;
      dst(1, 0) = ( - src(1, 0) * b5 + src(1, 2) * b2 - src(1, 3) * b1) * invdet;
      dst(2, 0) = ( + src(1, 0) * b4 - src(1, 1) * b2 + src(1, 3) * b0) * invdet;
      dst(3, 0) = ( - src(1, 0) * b3 + src(1, 1) * b1 - src(1, 2) * b0) * invdet;
      dst(0, 1) = ( - src(0, 1) * b5 + src(0, 2) * b4 - src(0, 3) * b3) * invdet;
      dst(1, 1) = ( + src(0, 0) * b5 - src(0, 2) * b2 + src(0, 3) * b1) * invdet;
      dst(2, 1) = ( - src(0, 0) * b4 + src(0, 1) * b2 - src(0, 3) * b0) * invdet;
      dst(3, 1) = ( + src(0, 0) * b3 - src(0, 1) * b1 + src(0, 2) * b0) * invdet;
      dst(0, 2) = ( + src(3, 1) * a5 - src(3, 2) * a4 + src(3, 3) * a3) * invdet;
      dst(1, 2) = ( - src(3, 0) * a5 + src(3, 2) * a2 - src(3, 3) * a1) * invdet;
      dst(2, 2) = ( + src(3, 0) * a4 - src(3, 1) * a2 + src(3, 3) * a0) * invdet;
      dst(3, 2) = ( - src(3, 0) * a3 + src(3, 1) * a1 - src(3, 2) * a0) * invdet;
      dst(0, 3) = ( - src(2, 1) * a5 + src(2, 2) * a4 - src(2, 3) * a3) * invdet;
      dst(1, 3) = ( + src(2, 0) * a5 - src(2, 2) * a2 + src(2, 3) * a1) * invdet;
      dst(2, 3) = ( - src(2, 0) * a4 + src(2, 1) * a2 - src(2, 3) * a0) * invdet;
      dst(3, 3) = ( + src(2, 0) * a3 - src(2, 1) * a1 + src(2, 2) * a0) * invdet; 
    }
  };


// -------------------------------------------------------------------------- //
// CommaInitializer
// -------------------------------------------------------------------------- //
  template<class M>
  class CommaInitializer {
  public:
    typedef typename Traits<M>::value_type value_type;

    CommaInitializer(M& m, const value_type& s): m(m), r(0), c(1) {
      this->m(0, 0) = s;
    }

    FORCEINLINE CommaInitializer& operator,(const value_type& s) {
      if (this->c == this->m.cols()) {
        this->r += 1;
        this->c = 0;
        assert(this->r < this->m.rows());
      }
      assert(this->c < this->m.cols());
      this->m(this->r, this->c++) = s;
      return *this;
    }

    M& finished() {
      return m; 
    }

    ~CommaInitializer() {
      assert(this->r + 1 == this->m.rows() && this->c == this->m.cols());
    }

  private:
    M& m;
    int r, c;
  };

  template<class Derived>
  inline CommaInitializer<Derived> operator<< (MatrixBase<Derived>& m, const typename Traits<Derived>::value_type& s) {
    return CommaInitializer<Derived>(m.derived(), s);
  }

  template<class Derived, class OtherDerived>
  inline CommaInitializer<Derived> operator<<(MatrixBase<Derived>& m, const MatrixBase<OtherDerived>& that) {
    return CommaInitializer<Derived>(m.derived(), that);
  }


// -------------------------------------------------------------------------- //
// Unroller
// -------------------------------------------------------------------------- //
  template<class L, class R, class O, class LParam, class RParam, class OParam,
          bool unroll = (Traits<L>::IsStaticallySized || Traits<R>::IsStaticallySized) && 
          ((Traits<L>::CoeffReadCost + Traits<R>::CoeffReadCost + FunctorTraits<O>::Cost) * 
          (Traits<L>::IsStaticallySized ? Traits<L>::SizeAtCompileTime : Traits<R>::SizeAtCompileTime)
          < ARX_UNROLL_COST_LIMIT),
          bool unrollLinearly = (Traits<L>::Flags & Traits<R>::Flags & LinearAccessBit) != 0> 
  struct Unroller {
    void operator()(LParam l, RParam r, OParam o) {
      assert(l.rows() == r.rows() && l.cols() == r.cols());
      for(int row = 0; row < l.rows(); ++row)
        for(int col = 0; col < l.cols(); ++col)
          o(l(row, col), r(row, col));
    }
  };

  template<class L, class R, class O, class LParam, class RParam, class OParam>
  struct Unroller<L, R, O, LParam, RParam, OParam, true, true> {
  private:
    enum {
      SizeAtCompileTime = Traits<L>::IsStaticallySized ? Traits<L>::SizeAtCompileTime : Traits<R>::SizeAtCompileTime
    };

    template<int index, int maxIndex> 
    struct unrollIteration {
      FORCEINLINE void operator()(LParam l, RParam r, OParam o) {
        o(l[index], r[index]);
        unrollIteration<index + 1, maxIndex>()(l, r, o);
      }
    };

    template<int end> 
    struct unrollIteration<end, end> {
      FORCEINLINE void operator()(LParam l, RParam r, OParam o) {}
    };

  public:
    FORCEINLINE void operator()(LParam l, RParam r, OParam o) {
      assert((l.rows() == r.rows() && l.cols() == r.cols()) || 
        (Traits<L>::IsVectorAtCompileTime && Traits<R>::IsVectorAtCompileTime && l.size() == r.size()));
      unrollIteration<0, SizeAtCompileTime>()(l, r, o);
    }
  };

  template<class L, class R, class O, class LParam, class RParam, class OParam>
  struct Unroller<L, R, O, LParam, RParam, OParam, true, false> {
  private:
    enum {
      ColsAtCompileTime = Traits<L>::IsStaticallySized ? Traits<L>::ColsAtCompileTime : Traits<R>::ColsAtCompileTime,
      RowsAtCompileTime = Traits<L>::IsStaticallySized ? Traits<L>::RowsAtCompileTime : Traits<R>::RowsAtCompileTime,
      SizeAtCompileTime = ColsAtCompileTime * RowsAtCompileTime
    };

    template<int index, int maxIndex> 
    struct unrollIteration {
      enum {
        row = (Traits<L>::Flags & RowMajorBit) ? index / ColsAtCompileTime : index % RowsAtCompileTime,
        col = (Traits<L>::Flags & RowMajorBit) ? index % ColsAtCompileTime : index / RowsAtCompileTime
      };

      FORCEINLINE void operator()(LParam l, RParam r, OParam o) {
        o(l(row, col), r(row, col));
        unrollIteration<index + 1, maxIndex>()(l, r, o);
      }
    };

    template<int end>
    struct unrollIteration<end, end> {
      FORCEINLINE void operator()(LParam l, RParam r, OParam o) {}
    };

  public:
    FORCEINLINE void operator()(LParam l, RParam r, OParam o) {
      assert(l.rows() == r.rows() && l.cols() == r.cols());
      unrollIteration<0, SizeAtCompileTime>()(l, r, o);
    }
  };


// -------------------------------------------------------------------------- //
// Cwise
// -------------------------------------------------------------------------- //
  template<class M>
  class Cwise {
  private:
    const M& m; /* TODO: may nest by value... */

  public:
    typedef typename Traits<M>::value_type value_type;

    Cwise(const M& m) : m(m) {}

#define ARX_CWISE_DEFINE_UNOP(NAME, FUNCTOR)                                    \
    const CwiseUnaryOp<M, FUNCTOR<value_type> >                                 \
    NAME() const {                                                              \
      return CwiseUnaryOp<M, FUNCTOR<value_type> >(this->m);                    \
    }

#define ARX_CWISE_DEFINE_BINOP(NAME, FUNCTOR)                                   \
    template<typename OtherDerived>                                             \
    const CwiseBinaryOp<M, OtherDerived, FUNCTOR<value_type> >                  \
    NAME(const MatrixBase<OtherDerived> &that) const {                          \
      return CwiseBinaryOp<M, OtherDerived, FUNCTOR<value_type> >(this->m, that.derived()); \
    }

    ARX_CWISE_DEFINE_UNOP(abs, Abs)
    ARX_CWISE_DEFINE_BINOP(min, Min)
    ARX_CWISE_DEFINE_BINOP(max, Max)
    ARX_CWISE_DEFINE_BINOP(operator<, std::less)
    ARX_CWISE_DEFINE_BINOP(operator<=, std::less_equal)
    ARX_CWISE_DEFINE_BINOP(operator>, std::greater)
    ARX_CWISE_DEFINE_BINOP(operator>=, std::greater_equal)
    ARX_CWISE_DEFINE_BINOP(operator==, std::equal_to)
    ARX_CWISE_DEFINE_BINOP(operator!=, std::not_equal_to)
    ARX_CWISE_DEFINE_BINOP(operator*, Mul)
    ARX_CWISE_DEFINE_BINOP(operator+, Add)
    ARX_CWISE_DEFINE_BINOP(operator/, Div)
    ARX_CWISE_DEFINE_BINOP(operator/, Sub)
#undef ARX_CWISE_DEFINE_BINOP
  };


// -------------------------------------------------------------------------- //
// MatrixBase
// -------------------------------------------------------------------------- //
  template<class Derived>
  class MatrixBase {
  public:
    typedef typename Traits<Derived>::value_type value_type;

    enum {
      RowsAtCompileTime = Traits<Derived>::RowsAtCompileTime,
      ColsAtCompileTime = Traits<Derived>::ColsAtCompileTime,
      SizeAtCompileTime = Traits<Derived>::SizeAtCompileTime,
      IsStaticallySized = Traits<Derived>::IsStaticallySized,
      IsDynamicallySized = Traits<Derived>::IsDynamicallySized,
      Flags = Traits<Derived>::Flags
    };

    enum {
      StorageOrder = Flags & RowMajorBit ? RowMajor : ColMajor
    };

    typedef Identity<value_type, RowsAtCompileTime, ColsAtCompileTime> IdentityReturnType;
    typedef CwiseNullaryOp<value_type, RowsAtCompileTime, ColsAtCompileTime, Random<value_type> > RandomReturnType;
    typedef Block<Derived, 1, ColsAtCompileTime> RowType;
    typedef Block<Derived, RowsAtCompileTime, 1> ColType;
    typedef typename Traits<Derived>::PlainMatrixType PlainMatrixType;

  public:
    typedef MatrixBase base_type;
    typedef Derived derived_type;

    Derived& derived() {
      return *static_cast<Derived*>(this);
    }

    const Derived& derived() const {
      return *static_cast<const Derived*>(this);
    }

    Derived& const_cast_derived() const {
      return *static_cast<Derived*>(const_cast<MatrixBase*>(this));
    }

    value_type& operator()(int r, int c) {
      return this->derived().coeff(r, c);
    }

    const value_type operator()(int r, int c) const {
      return this->derived().coeff(r, c);
    }

    value_type& operator[](int index) {
      return this->derived().coeff(index);
    }

    const value_type operator[](int index) const {
      return this->derived().coeff(index);
    }

    value_type& operator() (int index) {
      return this->derived().coeff(index);
    }

    const value_type operator() (int index) const {
      return this->derived().coeff(index);
    }

    int rows() const {
      return this->derived().rows();
    }

    int cols() const {
      return this->derived().cols();
    }

    int size() const {
      return rows() * cols();
    }

    template<typename OtherDerived>
    value_type dot(const MatrixBase<OtherDerived>& that) const {
      ARX_STATIC_ASSERT_VECTOR_ONLY(Derived);
      ARX_STATIC_ASSERT_VECTOR_ONLY(OtherDerived);
      ARX_STATIC_ASSERT_SAME_VALUETYPE(Derived, OtherDerived);
      ARX_STATIC_ASSERT_SAME_VECTOR_SIZE(Derived, OtherDerived);
      assert(size() == that.size());
      Dot<value_type> dot;
      Unroller<Derived, OtherDerived, Dot<value_type>, const Derived&, const OtherDerived&, Dot<value_type>&>
        ()(this->derived(), that.derived(), dot);
      return dot.get();
    }

    value_type squaredNorm() const {
      return this->dot(*this);
    }

    value_type norm() const {
      return sqrt(squaredNorm());
    }

    void normalize() {
      *this /= norm();
    }

    template<typename OtherDerived>
    PlainMatrixType cross(const MatrixBase<OtherDerived>& other) const {
      STATIC_ASSERT((Traits<Derived>::SizeAtCompileTime == 3));
      STATIC_ASSERT((Traits<OtherDerived>::SizeAtCompileTime == 3));

      /* There is no real need for expression here - compiler will do all the optimizations */
      typename Nested<Derived, 2>::type l(derived());
      typename Nested<OtherDerived, 2>::type r(other.derived());
      
      PlainMatrixType result;
      result[0] = l[1] * r[2] - l[2] * r[1];
      result[1] = l[2] * r[0] - l[0] * r[2];
      result[2] = l[0] * r[1] - l[1] * r[0];
      return result;
    }


    void setConstant(const value_type& value) {
      *this = CwiseNullaryOp<value_type, RowsAtCompileTime, ColsAtCompileTime, Constant<value_type> >
        (this->rows(), this->cols(), Constant<value_type>(value));
    }

    ColType col(int c) {
      return Block<Derived, RowsAtCompileTime, 1>(this->derived(), 0, c);
    }

    const ColType col(int c) const {
      return Block<Derived, RowsAtCompileTime, 1>(this->derived(), 0, c);
    }

    RowType row(int r) {
      return RowType(this->derived(), r, 0);
    }

    const RowType row(int r) const {
      return RowType(this->derived(), r, 0);
    }

    template<int blockRows, int blockCols>
    Block<Derived, blockRows, blockCols> block(int startRow, int startCol) {
      return Block<Derived, blockRows, blockCols>(this->derived(), startRow, startCol);
    }

    template<int blockRows, int blockCols>
    const Block<Derived, blockRows, blockCols> block(int startRow, int startCol) const {
      return Block<Derived, blockRows, blockCols>(this->derived(), startRow, startCol);
    }

    Block<Derived, DYNAMIC_SIZE, DYNAMIC_SIZE> block(int startRow, int startCol, int blockRows, int blockCols) {
      return Block<Derived, DYNAMIC_SIZE, DYNAMIC_SIZE>(this->derived(), startRow, startCol, blockRows, blockCols);
    }

    const Block<Derived, DYNAMIC_SIZE, DYNAMIC_SIZE> block(int startRow, int startCol, int blockRows, int blockCols) const {
      return Block<Derived, DYNAMIC_SIZE, DYNAMIC_SIZE>(this->derived(), startRow, startCol, blockRows, blockCols);
    }

    Transpose<Derived> transpose() {
      return Transpose<Derived>(this->derived());
    }

    const Transpose<Derived> transpose() const {
      return Transpose<Derived>(this->derived());
    }

    static const IdentityReturnType Identity(int r, int c) {
      return IdentityReturnType(r, c);
    }

    static const IdentityReturnType Identity() {
      STATIC_ASSERT((IsStaticallySized));
      return IdentityReturnType(ColsAtCompileTime, RowsAtCompileTime);
    }

    Derived& setRandom() {
      return *this = random();
    }

    static const RandomReturnType random() {
      STATIC_ASSERT((IsStaticallySized));
      return RandomReturnType();
    }

    static const RandomReturnType random(int size) {
      return RandomReturnType(size);
    }

    static const RandomReturnType random(int r, int c) {
      return RandomReturnType(r, c);
    }

    const Cwise<Derived> cwise() const {
      return Cwise<Derived>(this->derived());
    }

    Cwise<Derived> cwise() {
      return Cwise<Derived>(this->derived());
    }

    bool all() const {
      for(int j = 0; j < cols(); ++j)
        for(int i = 0; i < rows(); ++i)
          if (!this->derived().coeff(i, j)) return false;
      return true;
    }

    bool any() const {
      for(int j = 0; j < this->cols(); ++j)
        for(int i = 0; i < this->rows(); ++i)
          if (this->derived().coeff(i, j)) return true;
      return false;
    }

    template<typename OtherDerived>
    Derived& operator= (const MatrixBase<OtherDerived>& that) {
      ARX_STATIC_ASSERT_SAME_VALUETYPE(Derived, OtherDerived);
      STATIC_ASSERT((ARX_SAME_MATRIX_SIZE(Derived, OtherDerived) || 
        (ARX_VECTOR_ONLY(Derived) && ARX_VECTOR_ONLY(OtherDerived) && ARX_SAME_VECTOR_SIZE(Derived, OtherDerived))));
      assert(this->rows() == that.rows() && this->cols() == that.cols());
      Unroller<Derived, OtherDerived, Assign<value_type>, Derived&, const OtherDerived&, const Assign<value_type>&>
        ()(this->derived(), that.derived(), Assign<value_type>());
      return this->derived();
    }

    Derived& operator= (const MatrixBase& that) {
      return this->operator=<Derived>(that);
    }

    template<typename OtherDerived>
    Derived& operator+= (const MatrixBase<OtherDerived>& that) {
      return *this = *this + that;
    }

    template<typename OtherDerived>
    Derived& operator-= (const MatrixBase<OtherDerived>& that) {
      return *this = *this - that;
    }

    template<typename OtherDerived>
    Derived& operator*= (const MatrixBase<OtherDerived>& that) {
      return *this = *this * that;
    }

    Derived& operator*= (const value_type& that) {
      return *this = *this * that;
    }
    
    Derived& operator/= (const value_type& that) {
      return *this = *this / that;
    }

    template<class OtherDerived>
    void computeInverse(MatrixBase<OtherDerived> &result) const {
      ARX_STATIC_ASSERT_SAME_MATRIX_SIZE(Derived, OtherDerived);
      ARX_STATIC_ASSERT_DYNAMIC_NUMBERS_EQ(RowsAtCompileTime, ColsAtCompileTime);
      STATIC_ASSERT((NumTraits<value_type>::HasFloatingPoint));
      assert(this->rows() == this->cols());
      assert(this->rows() == result.rows() && this->cols() == result.cols());
      Inverter<RowsAtCompileTime>()(*this, result);
    }

    const PlainMatrixType inverse() const {
      PlainMatrixType result(rows(), cols());
      computeInverse(result);
      return result;
    }
  };

  template<class Derived, class CharT, class CharTraits>
  std::basic_ostream<CharT, CharTraits>& operator<< (std::basic_ostream<CharT, CharTraits>& s, const MatrixBase<Derived>& m) {
    if(Traits<Derived>::IsVectorAtCompileTime) {
      s << m[0];
      for(int i = 1; i < m.size(); i++)
        s << " " << m[i];
    } else {
      for(int r = 0; r < m.rows(); r++) {
        s << m(r, 0);
        for(int c = 1; c < m.cols(); c++)
          s << " " << m(r, c);
        if(r < m.rows() - 1)
          s << std::endl;
      }
    }
    return s;
  }


// -------------------------------------------------------------------------- //
// MatrixStorage
// -------------------------------------------------------------------------- //
  struct StackStorage {
    template<class T, int R, int C>
    class apply {
    public:
      ARX_VEC_ALIGNED T storedData[R * C];

      apply() {}

      apply(int rows, int cols) {}

      int rows() const { 
        return R; 
      }

      int cols() const { 
        return C; 
      }
    };
  };

  struct HeapStorage {
    template<class T, int R, int C>
    class apply {
    private:
      int myRows;
      int myCols;

    public:
      T* storedData;

      apply(): storedData(NULL) {}

      apply(int rows, int cols): myRows(rows), myCols(cols) {
        this->storedData = static_cast<T*>
          (ARX_VEC_ALIGNED_MALLOC(this->rowsStorage.getValue() * this->colsStorage.getValue() * sizeof(T)));
      }

      ~apply() {
        ARX_VEC_ALIGNED_FREE(this->storedData);
      }

      int rows() const { 
        return this->myRows; 
      }

      int cols() const { 
        return this->myCols; 
      }
    };
  };

  struct RowMajorStorage {
    template<class Base>
    class apply: public Base {
    public:
      typedef apply base_type;

      int dataOffset(int r, int c) const {
        return r * cols() + c;
      }

      apply() {}

      apply(int rows, int cols): Base(rows, cols) {}
    };
  };

  struct ColMajorStorage {
    template<class Base>
    class apply: public Base {
    public:
      typedef apply base_type;

      int dataOffset(int r, int c) const {
        return c * rows() + r;
      }

      apply() {}

      apply(int rows, int cols): Base(rows, cols) {}
    };
  };

  template<class T, int R, int C, int StorageOrder> class MatrixStorage: public
    if_c<StorageOrder == RowMajor, RowMajorStorage, ColMajorStorage>::type::
    apply<if_c<R == DYNAMIC_SIZE || C == DYNAMIC_SIZE, HeapStorage, StackStorage>::type::apply<T, R, C> > 
  {
  public:
    MatrixStorage() {}

    MatrixStorage(int rows, int cols): base_type(rows, cols) {}

    T* data() {
      return this->storedData;
    }

    const T* data() const {
      return this->storedData;
    }

    T& coeff(int r, int c) { 
      assert(r < this->rows() && c < this->cols() && r >= 0 && c >= 0);
      return this->data()[this->dataOffset(r, c)]; 
    }

    const T coeff(int r, int c) const {
      assert(r < this->rows() && c < this->cols() && r >= 0 && c >= 0);
      return this->data()[this->dataOffset(r, c)]; 
    }

    T& coeff(int index) {
      assert(index < this->rows() * this->cols() && index >= 0);
      return this->data()[index];
    }

    const T coeff(int index) const {
      assert(index < this->rows() * this->cols() && index >= 0);
      return this->data()[index];
    }
  };


// -------------------------------------------------------------------------- //
// Matrix
// -------------------------------------------------------------------------- //
  template<class T, int R, int C, int StorageOrder = ColMajor>
  class Matrix: public MatrixBase<Matrix<T, R, C, StorageOrder> > {
  private:
    typedef Matrix this_type;

    ARX_BEFRIEND_MATRIXBASE();

    MatrixStorage<T, R, C, StorageOrder> storage;

  public:
    Matrix() {}

    Matrix(int r, int c): storage(r, c) {}

    Matrix(const value_type& x, const value_type& y, const value_type& z): storage(RowsAtCompileTime, ColsAtCompileTime) {
      ARX_STATIC_ASSERT_VECTOR_ONLY(this_type);
      STATIC_ASSERT((SizeAtCompileTime == 3));
      this->coeff(0) = x;
      this->coeff(1) = y;
      this->coeff(2) = z;
    }

    Matrix(const value_type& x, const value_type& y, const value_type& z, const value_type& w): storage(RowsAtCompileTime, ColsAtCompileTime) {
      ARX_STATIC_ASSERT_VECTOR_ONLY(this_type);
      STATIC_ASSERT((SizeAtCompileTime == 4));
      this->coeff(0) = x;
      this->coeff(1) = y;
      this->coeff(2) = z;
      this->coeff(3) = w;
    }

    ARX_INHERIT_ASSIGNMENT_OPERATORS();

    template<class OtherDerived>
    Matrix(const MatrixBase<OtherDerived>& that): storage(that.rows(), that.cols()) {
      *this = that;
    }

    Matrix(const Matrix& that): storage(that.rows(), that.cols()) {
      *this = that;
    }

    int rows() const {
      return this->storage.rows(); 
    }

    int cols() const {
      return this->storage.cols(); 
    }

    value_type& coeff(int r, int c) {
      return this->storage.coeff(r, c);
    }

    const value_type coeff(int r, int c) const { 
      return this->storage.coeff(r, c);
    }

    value_type& coeff(int index) {
      return this->storage.coeff(index);
    }

    const value_type coeff(int index) const { 
      return this->storage.coeff(index);
    }

    T* data() {
      return this->storage.data();
    }

    const T* data() const {
      return this->storage.data();
    }
  };

// -------------------------------------------------------------------------- //
// CwiseBinaryOp
// -------------------------------------------------------------------------- //
  template<class L, class R, class O>
  class CwiseBinaryOp: public MatrixBase<CwiseBinaryOp<L, R, O> >, private nonassignable {
  private:
    typedef CwiseBinaryOp this_type;

    typename Traits<this_type>::LNested l;
    typename Traits<this_type>::RNested r;
    const O o;

  public:
    CwiseBinaryOp(const L& l, const R& r, const O& o = O()): l(l), r(r), o(o) {
      assert(l.cols() == r.cols() && l.rows() == r.rows());
    }

    // default copy constructor is OK
    
    int rows() const {
      return this->l.rows();
    }

    int cols() const {
      return this->l.cols();
    }

    const value_type coeff(int r, int c) const { 
      return this->o(this->l(r, c), this->r(r, c));
    }

    const value_type coeff(int index) const { 
      return this->o(this->l[index], this->r[index]);
    }
  };

  template<class L, class R>
  FORCEINLINE const CwiseBinaryOp<L, R, Add<typename Traits<L>::value_type> > operator+ (const MatrixBase<L>& l, const MatrixBase<R>& r) {
    return CwiseBinaryOp<L, R, Add<typename Traits<L>::value_type> >(l.derived(), r.derived());
  }

  template<class L, class R>
  FORCEINLINE const CwiseBinaryOp<L, R, Sub<typename Traits<L>::value_type> > operator- (const MatrixBase<L>& l, const MatrixBase<R>& r) {
    return CwiseBinaryOp<L, R, Sub<typename Traits<L>::value_type> >(l.derived(), r.derived());
  }


// -------------------------------------------------------------------------- //
// CwiseUnaryOp
// -------------------------------------------------------------------------- //
  template<class M, class O>
  class CwiseUnaryOp: public MatrixBase<CwiseUnaryOp<M, O> >, private nonassignable {
  private:
    typedef CwiseUnaryOp this_type;

    typename Traits<this_type>::MNested m;
    const O o;

  public:
    CwiseUnaryOp(const M& m, const O& o = O()): m(m), o(o) {}

    // default copy constructor is OK

    int rows() const {
      return this->m.rows();
    }

    int cols() const {
      return this->m.cols();
    }

    const value_type coeff(int r, int c) const { 
      return this->o(this->m(r, c));
    }

    const value_type coeff(int index) const {
      return this->o(this->m[index]);
    }
  };

  template<class M>
  FORCEINLINE const CwiseUnaryOp<M, Neg<typename Traits<M>::value_type> > operator- (const MatrixBase<M>& m) {
    return CwiseUnaryOp<M, Neg<typename Traits<M>::value_type> >(m.derived());
  }

  template<class M>
  FORCEINLINE const CwiseUnaryOp<M, Peq<typename Traits<M>::value_type> > operator+ (const MatrixBase<M>& m) {
    return CwiseUnaryOp<M, Peq<typename Traits<M>::value_type> >(m.derived());
  }

  template<class M>
  FORCEINLINE const CwiseUnaryOp<M, MulC<typename Traits<M>::value_type> > operator* (const MatrixBase<M>& l, const typename Traits<M>::value_type& r) {
    return CwiseUnaryOp<M, MulC<typename Traits<M>::value_type> >(l.derived(), MulC<typename Traits<M>::value_type>(r));
  }

  template<class M>
  FORCEINLINE const CwiseUnaryOp<M, PreMulC<typename Traits<M>::value_type> > operator* (const typename Traits<M>::value_type& l, const MatrixBase<M>& r) {
    return CwiseUnaryOp<M, PreMulC<typename Traits<M>::value_type> >(r.derived(), PreMulC<typename Traits<M>::value_type>(l));
  }

  template<class M>
  FORCEINLINE const CwiseUnaryOp<M, DivC<typename Traits<M>::value_type> > operator/ (const MatrixBase<M>& l, const typename Traits<M>::value_type& r) {
    return CwiseUnaryOp<M, DivC<typename Traits<M>::value_type> >(l.derived(), DivC<typename Traits<M>::value_type>(r));
  }


// -------------------------------------------------------------------------- //
// CwiseNullaryOp
// -------------------------------------------------------------------------- //
  template<class T, int R, int C, class O> 
  class CwiseNullaryOp: public MatrixBase<CwiseNullaryOp<T, R, C, O> >, private nonassignable {
  private:
    typedef CwiseNullaryOp this_type;

    int_if_dynamic<R> rowsStorage;
    int_if_dynamic<C> colsStorage;
    const O o;

  public:
    CwiseNullaryOp(const O& o = O()): o(o), rowsStorage(R), colsStorage(C) {
      STATIC_ASSERT((R != DYNAMIC_SIZE && C != DYNAMIC_SIZE));
    }

    CwiseNullaryOp(int size, const O& o = O()): 
        o(o), rowsStorage(R == DYNAMIC_SIZE ? size : 1), colsStorage(C == DYNAMIC_SIZE ? size : 1) {
      STATIC_ASSERT((R == 1 || C == 1));
      assert(size >= 1);
    }

    CwiseNullaryOp(int rows, int cols, const O& o = O()): o(o), rowsStorage(rows), colsStorage(cols) {
      assert((R == DYNAMIC_SIZE || R == rows) && (C == DYNAMIC_SIZE || C == cols));
      assert(rows >= 1 && cols >= 1);
    }

    // default copy constructor is OK

    int rows() const {
      return this->rowsStorage.getValue();
    }

    int cols() const {
      return this->colsStorage.getValue();
    }

    const value_type coeff(int r, int c) const { 
      return this->o();
    }

    const value_type coeff(int index) const {
      return this->o();
    }
  };


// -------------------------------------------------------------------------- //
// MatrixMul
// -------------------------------------------------------------------------- //
  template<class L, class R>
  class MatrixMul: public MatrixBase<MatrixMul<L, R> >, private nonassignable {
  private:
    typedef MatrixMul this_type;

    typename Traits<this_type>::LNested l;
    typename Traits<this_type>::RNested r;

  public:
    MatrixMul(const L& l, const R& r): l(l), r(r) {
      assert(l.cols() == r.rows());
    }

    // default copy constructor is OK

    int rows() const {
      return this->l.rows();
    }

    int cols() const {
      return this->r.cols();
    }

    const value_type coeff(int r, int c) const {
      return this->l.row(r).transpose().dot(this->r.col(c));
    }
  };

  template<class L, class R>
  FORCEINLINE const MatrixMul<L, R> operator* (const MatrixBase<L>& l, const MatrixBase<R>& r) {
    return MatrixMul<L, R>(l.derived(), r.derived());
  }


// -------------------------------------------------------------------------- //
// Transpose
// -------------------------------------------------------------------------- //
  template<class M>
  class Transpose: public MatrixBase<Transpose<M> > {
  private:
    typedef Transpose this_type;

    typename Traits<this_type>::MNested m;

  public:
    Transpose(const M& m): m(m) {}

    // default copy constructor is OK

    int rows() const {
      return this->m.cols();
    }

    int cols() const {
      return this->m.rows();
    }

    const value_type coeff(int r, int c) const {
      return this->m(c, r);
    }

    value_type& coeff(int r, int c) {
      return this->m.const_cast_derived()(c, r);
    }

    const value_type coeff(int index) const {
      return this->m(index);
    }

    value_type& coeff(int index) {
      return this->m.const_cast_derived()(index);
    }

    ARX_INHERIT_ASSIGNMENT_OPERATORS();
  };


// -------------------------------------------------------------------------- //
// Identity
// -------------------------------------------------------------------------- //
  template<class T, int R, int C> 
  class Identity: public MatrixBase<Identity<T, R, C> >, private nonassignable {
  private:
    int_if_dynamic<R> rowsStorage;
    int_if_dynamic<C> colsStorage;

  public:
    Identity(): rowsStorage(R), colsStorage(C) {}

    Identity(int rows, int cols): rowsStorage(rows), colsStorage(cols) {}

    int rows() const {
      return this->rowsStorage.getValue();
    }

    int cols() const {
      return this->colsStorage.getValue();
    }

    const value_type coeff(int r, int c) const {
      assert(r < this->rows() && c < this->cols() && r >= 0 && c >= 0);
      return (r == c) ? static_cast<value_type>(1) : static_cast<value_type>(0);
    }
  };


// -------------------------------------------------------------------------- //
// Block
// -------------------------------------------------------------------------- //
  template<class M, int R, int C> 
  class Block: public MatrixBase<Block<M, R, C> > {
  private:
    typedef Block this_type;

    int_if_dynamic<R> rowsStorage;
    int_if_dynamic<C> colsStorage;
    
    typename Traits<this_type>::MNested m;

    int_if_dynamic<Traits<M>::RowsAtCompileTime == 1 ? 0 : DYNAMIC_SIZE> startRow;
    int_if_dynamic<Traits<M>::ColsAtCompileTime == 1 ? 0 : DYNAMIC_SIZE> startCol;

  public:
    /** Constructor for fixed size */
    Block(const M& m, int startRow, int startCol): m(m), rowsStorage(R), colsStorage(C), startRow(startRow), startCol(startCol) {
      STATIC_ASSERT((R != DYNAMIC_SIZE && C != DYNAMIC_SIZE));
      assert(startRow >= 0 && startRow + R <= m.rows() && startCol >= 0 && startCol + C <= m.cols());
    }

    /** Constructor for dynamic size */
    Block(const M& m, int startRow, int startCol, int blockRows, int blockCols): m(m), rowsStorage(blockRows), colsStorage(blockCols), startRow(startRow), startCol(startCol) {
      assert((R == DYNAMIC_SIZE || R == blockRows) && (C == DYNAMIC_SIZE || C == blockCols));
      assert(startRow >= 0 && blockRows >= 1 && startRow + blockRows <= m.rows() && 
        startCol >= 0 && blockCols >= 1 && startCol + blockCols <= m.cols());
    }

    int rows() const {
      return this->rowsStorage.getValue();
    }

    int cols() const {
      return this->colsStorage.getValue();
    }

    const value_type coeff(int r, int c) const {
      return this->m(this->startRow.getValue() + r, this->startCol.getValue() + c);
    }

    value_type& coeff(int r, int c) {
      return this->m.const_cast_derived()(this->startRow.getValue() + r, this->startCol.getValue() + c);
    }

    const value_type coeff(int index) const {
      STATIC_ASSERT((Traits<this_type>::IsVectorAtCompileTime));
      return this->m(startRow.getValue() + (R == 1 ? 0 : index), startCol.getValue() + (C == 1 ? 0 : index));
    }

    value_type& coeff(int index) {
      STATIC_ASSERT((Traits<this_type>::IsVectorAtCompileTime));
      return this->m.const_cast_derived()(startRow.getValue() + (R == 1 ? 0 : index), startCol.getValue() + (C == 1 ? 0 : index));
    }
  };


// -------------------------------------------------------------------------- //
// LinearSystemSolver
// -------------------------------------------------------------------------- //
  template<class A, class B, bool UseDirectPermutations>
  struct LinearSystemSolver {
    void operator()(MatrixBase<A>& a, MatrixBase<B>& b) const {
      typedef Traits<A>::value_type value_type;

      // Triangulize the matrix.
      for(int col = 0; col < b.size() - 1; col++) {
        // Move row with largest coefficient to top.
        value_type maxc = std::numeric_limits<value_type>::min();
        int pivot = 0;
        for(int row = col; row < b.size(); row++) {
          value_type coef = abs(a[row][col]);
          if(coef > maxc) {
            maxc = coef;
            pivot = row;
          }
        }

        // Exchange "pivot" with "col" row 
        if(pivot != col) {
          for(int i = 0; i < a.cols(); i++)
            std::swap(a(pivot, i), a(col, i));
          std::swap(b[pivot], b[col]);
        }

        // Do reduction for this column.
        for(int row = col + 1; row < b.size(); row++) {
          value_type mult = a[row][col] / a[col][col];
          for(int c = col; c < b.size(); c++)  // Could start with c=col+1?
            a[row][c] -= mult * a[col][c];
          b[row] -= mult * b[col];
        }
      }

      // Do back substitution.  Pivoting does not affect solution order.
      for(int row = (int) b.size() - 1; row >= 0; row--) {
        value_type val = b[row];
        for(int col = (int) b.size() - 1; col > row; col--)
          val -= b[col] * a[row][col];
        b[row] = val / a[row][row];
      }
    }
  };

  template<class A, class B>
  struct LinearSystemSolver<A, B, false> {
    void operator()(MatrixBase<A>& a, MatrixBase<B>& b) const {
      typedef Traits<A>::value_type value_type;

      // construct permutation vector
      Matrix<int, Traits<A>::RowsAtCompileTime, 1> p(a.rows(), 1);
      for(int i = 0; i < a.rows(); i++)
        p[i] = i;

      // Triangulize the matrix.
      for(int col = 0; col < b.size() - 1; col++) {
        // Move row with largest coefficient to top.
        value_type maxc = std::numeric_limits<value_type>::min();
        int pivot = 0;
        for(int row = col; row < b.size(); row++) {
          value_type coef = abs(a[p[row]][col]);
          if(coef > maxc) {
            maxc = coef;
            pivot = row;
          }
        }

        // Exchange "pivot" with "col" row 
        if(pivot != col) {
          std::swap(p[pivot], p[col]);
          std::swap(b[pivot], b[col]);
        }

        // Do reduction for this column.
        for(int row = col + 1; row < b.size(); row++) {
          value_type mult = a[p[row]][col] / a[p[col]][col];
          for(int c = col; c < b.size(); c++)  // Could start with c=col+1?
            a[p[row]][c] -= mult * a[p[col]][c];
          b[row] -= mult * b[col];
        }
      }

      // Do back substitution
      for(int row = (int) b.size() - 1; row >= 0; row--) {
        value_type val = b[row];
        for(int col = (int) b.size() - 1; col > row; col--)
          val -= b[col] * a[p[row]][col];
        b[row] = val / a[p[row]][row];
      }
    }
  };

  template<class A, class B> 
  void solveLinearSystem(MatrixBase<A>& a, MatrixBase<B>& b) {
    ARX_STATIC_ASSERT_SAME_VALUETYPE(A, B);
    ARX_STATIC_ASSERT_VECTOR_ONLY(B);
    ARX_STATIC_ASSERT_DYNAMIC_NUMBERS_EQ(Traits<A>::RowsAtCompileTime, Traits<B>::SizeAtCompileTime);
    assert(a.rows() == b.size());
    LinearSystemSolver<A, B, Traits<A>::IsStaticallySized && 
      Traits<B>::IsStaticallySized && Traits<A>::ColsAtCompileTime < 5>()(a, b);
  }


// -------------------------------------------------------------------------- //
// Useful typedefs
// -------------------------------------------------------------------------- //
  typedef Matrix<float, 4, 1> Vector4f;
  typedef Matrix<float, 3, 1> Vector3f;
  typedef Matrix<float, 2, 1> Vector2f;

  typedef Matrix<float, 4, 4> Matrix4f;
  typedef Matrix<float, 3, 3> Matrix3f;
  typedef Matrix<float, 2, 2> Matrix2f;

  typedef Matrix<float, DYNAMIC_SIZE, 1> VectorXf;
  typedef Matrix<float, DYNAMIC_SIZE, DYNAMIC_SIZE> MatrixXf;

} // namespace arx

/* Clean up. */
#ifdef MIN_UNDEFFED
#  define min(x, y) ((x > y)?(y):(x))
#  undef MIN_UNDEFFED
#endif
#ifdef MAX_UNDEFFED
#  define max(x, y) ((x > y)?(x):(y))
#  undef MAX_UNDEFFED
#endif

#undef ARX_INHERIT_ASSIGNMENT_OPERATOR
#undef ARX_INHERIT_SCALAR_ASSIGNMENT_OPERATOR
#undef ARX_INHERIT_ASSIGNMENT_OPERATORS_FROM
#undef ARX_INHERIT_ASSIGNMENT_OPERATORS
#undef ARX_BEFRIEND_MATRIXBASE_WPREFIX
#undef ARX_BEFRIEND_MATRIXBASE

#undef ARX_DYNAMIC_NUMBERS_EQ
#undef ARX_SAME_MATRIX_SIZE
#undef ARX_SAME_VECTOR_SIZE
#undef ARX_VECTOR_ONLY
#undef ARX_STATIC_ASSERT_DYNAMIC_NUMBERS_EQ
#undef ARX_STATIC_ASSERT_SAME_MATRIX_SIZE
#undef ARX_STATIC_ASSERT_SAME_VECTOR_SIZE
#undef ARX_STATIC_ASSERT_VECTOR_ONLY
#undef ARX_STATIC_ASSERT_SAME_VALUETYPE

#undef ARX_VEC_ALIGNMENT
#undef ARX_VEC_ALIGNED
#undef ARX_VEC_ALIGNED_MALLOC
#undef ARX_VEC_ALIGNED_FREE

#endif // ARX_USE_EIGEN

#endif // __ARX_LINEARALGEBRA_H__
