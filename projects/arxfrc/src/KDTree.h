#ifndef __KDTREE_H__
#define __KDTREE_H__

#include <limits>
#include "ArXMath.h"

template<class ParamIndexType, int ParamDimensionCount, class ParamDataType>
struct KDTreeItem
{
	ParamIndexType Index[ParamDimensionCount];
	ParamDataType Data;
};

template<class ParamIndexType, int ParamDimensionCount, class ParamDataType>
struct KDTreeItemContainer
{
	KDTreeItem<ParamIndexType, ParamDimensionCount, ParamDataType> Item;
	int Left, Right;
};

template<class ParamIndexType, int ParamDimensionCount, class ParamDataType>
class TKDTree
{
	//TODO: add consts to methods
public:
	static const int DimensionCount = ParamDimensionCount;
	typedef ParamIndexType IndexType;
	typedef ParamDataType DataType;
	typedef KDTreeItem<ParamIndexType, ParamDimensionCount, ParamDataType> KDTreeItem;
	typedef KDTreeItemContainer<ParamIndexType, ParamDimensionCount, ParamDataType> KDTreeItemContainer;

	int GetCount() {return Count;}
	TKDTree(int n, KDTreeItem* Data);
	~TKDTree() {};
	void FindInRadius(IndexType* Center, IndexType Radius);
	void FindClosePoint(IndexType* Point);
	void FindClosestPoint(IndexType* Point);
	void FindClosestPoints(IndexType* Point, int NeededCount);
	int FoundCount;
	KDTreeItem** Found;
private:
	IndexType SqrDist(IndexType* V1, IndexType* V2);
	void AddToSortedDistances(IndexType Dist2);
	void DoFindInRadius(int Node, int Idx);
	void DoFindClosestPoint(int Node, int Idx);
	void DoFindClosestPoints(int Node, int Idx);
	int BuildTree(int Lo, int Hi, int Idx);
	void Medianize(int Lo, int Hi, int Idx, int K);
	KDTreeItemContainer** Items;
	IndexType* DistancesSqr;
	IndexType* SortedDistancesSqr;
	IndexType SearchRadiusSqr;
	IndexType SearchRadius;
	IndexType* SearchCenter;
	int SearchCount;
	int Count;
	int Root;
};

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType 
TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::SqrDist
  (typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* V1, 
	typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* V2)
{
	IndexType Result = 0;
	for(int i = 0; i < DimensionCount; i++)
		Result += sqr(V1[i] - V2[i]);
	return Result;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::AddToSortedDistances
	(typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType Dist2)
{
	SortedDistancesSqr[FoundCount] = std::numeric_limits<IndexType>::max();
	for(int i = FoundCount; i > 0; i--)
	{
		if(SortedDistancesSqr[i - 1] > Dist2)
			SortedDistancesSqr[i] = SortedDistancesSqr[i - 1];
		else
		{
			SortedDistancesSqr[i] = Dist2;
			return;
		}
	}
	SortedDistancesSqr[0] = Dist2;
	return;
}


template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::DoFindClosestPoints(int Node, int Idx)
{
	int ChildIdx = Idx + 1;
	if(ChildIdx == DimensionCount)
		ChildIdx = 0;

	if(SearchCenter[Idx] < Items[Node]->Item.Index[Idx])
	{
		if(Items[Node]->Left != -1)
			DoFindClosestPoints(Items[Node]->Left, ChildIdx);

		IndexType Dist2 = SqrDist(Items[Node]->Item.Index, SearchCenter);
		if(Dist2 < SearchRadiusSqr)
		{
			AddToSortedDistances(Dist2);
			Found[FoundCount] = &Items[Node]->Item;
			DistancesSqr[FoundCount] = Dist2;
			SearchRadiusSqr = SortedDistancesSqr[SearchCount - 1];
			FoundCount++;
		}

		if(Items[Node]->Right != -1)
			if(sqr(Items[Node]->Item.Index[Idx] - SearchCenter[Idx]) < SearchRadiusSqr)
				DoFindClosestPoints(Items[Node]->Right, ChildIdx);
	}
	else
	{
		if(Items[Node]->Right != -1)
			DoFindClosestPoints(Items[Node]->Right, ChildIdx);

		IndexType Dist2 = SqrDist(Items[Node]->Item.Index, SearchCenter);
		if(Dist2 < SearchRadiusSqr)
		{
			AddToSortedDistances(Dist2);
			Found[FoundCount] = &Items[Node]->Item;
			DistancesSqr[FoundCount] = Dist2;
			SearchRadiusSqr = SortedDistancesSqr[SearchCount - 1];
			FoundCount++;
		}

		if(Items[Node]->Left != -1)
			if(sqr(Items[Node]->Item.Index[Idx] - SearchCenter[Idx]) < SearchRadiusSqr)
				DoFindClosestPoints(Items[Node]->Left, ChildIdx);
	}
	return;
} 

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::FindClosestPoints
	(typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* Point, int NeededCount)
{
	FoundCount = 0;
	SearchCenter = Point;
	SearchCount = NeededCount;
	if(SearchCount > Count)
		SearchCount = Count;
	SortedDistancesSqr[SearchCount - 1] = std::numeric_limits<IndexType>::max();
	SearchRadiusSqr = std::numeric_limits<IndexType>::max();
	DoFindClosestPoints(Root,0);
	int k = 0;
	for(int i = 0; i < FoundCount; i++)	if(DistancesSqr[i] <= SearchRadiusSqr)
	{
		Found[k] = Found[i];
		k++;
	}
	FoundCount = NeededCount;
	return;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::DoFindClosestPoint(int Node, int Idx)
{
	int ChildIdx = Idx + 1;
	if(ChildIdx == DimensionCount)
		ChildIdx = 0;

	if(SearchCenter[Idx] < Items[Node]->Item.Index[Idx])
	{
		if(Items[Node]->Left != -1)
			DoFindClosestPoint(Items[Node]->Left, ChildIdx);

		IndexType Dist2 = SqrDist(Items[Node]->Item.Index, SearchCenter);
		if(SearchRadiusSqr > Dist2)
		{
			Found[0] = &Items[Node]->Item;
			SearchRadiusSqr = Dist2;
		}

		if(Items[Node]->Right != -1)
			if(sqr(Items[Node]->Item.Index[Idx] - SearchCenter[Idx]) < SearchRadiusSqr)
				DoFindClosestPoint(Items[Node]->Right, ChildIdx);
	}
	else
	{
		if(Items[Node]->Right != -1)
			DoFindClosestPoint(Items[Node]->Right, ChildIdx);

		IndexType Dist2 = SqrDist(Items[Node]->Item.Index, SearchCenter);
		if(SearchRadiusSqr > Dist2)
		{
			Found[0] = &Items[Node]->Item;
			SearchRadiusSqr = Dist2;
		}

		if(Items[Node]->Left != -1)
			if(sqr(Items[Node]->Item.Index[Idx] - SearchCenter[Idx]) < SearchRadiusSqr)
				DoFindClosestPoint(Items[Node]->Left, ChildIdx);
	}
	return;
} 

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::FindClosestPoint
  (typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* Point)
{
	SearchRadiusSqr = std::numeric_limits<IndexType>::max();
	SearchCenter = Point;
	DoFindClosestPoint(Root, 0);
	FoundCount = 1;
	return;
} 

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::FindClosePoint
  (typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* Point)
{
	int Node = Root, NewNode;
	int Idx = 0;
	while(true)
	{
		if(Point[Idx] < Items[Node]->Item.Index[Idx])
			NewNode = Items[Node]->Left;
		else
			NewNode = Items[Node]->Right;
		
		if(NewNode == -1)
			break;
		Node = NewNode;

		Idx++;
		if(Idx == DimensionCount)
			Idx = 0;
	}

	FoundCount = 1;
	Found[0] = &Items[Node]->Item;
	return;
} 

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::TKDTree
  (int n, typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::KDTreeItem* Data)
{
	Count = n;
	Items = new KDTreeItemContainer*[n];
	Found = new KDTreeItem*[n];
	DistancesSqr = new IndexType[n];
	SortedDistancesSqr = new IndexType[n];
	for(int i = 0; i < Count; i++)
	{
		Items[i] = new KDTreeItemContainer;
		Items[i]->Item = Data[i];
	}
	Root = BuildTree(0, Count - 1, 0);
	return;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::Medianize(int Lo, int Hi, int Idx, int K)
{
	int l,h;
	IndexType x;

	if(Lo == Hi)
		return;

	x = Items[K]->Item.Index[Idx];
	l = Lo;
	h = Hi;

	do{
		while(Items[l]->Item.Index[Idx] < x) l++;
		while(Items[h]->Item.Index[Idx] > x) h--;
		if(l <= h)
		{
			KDTreeItemContainer* y = Items[l];
			Items[l] = Items[h];
			Items[h] = y;
			l++;
			h--;
		}
	} while (l <= h);

	if (K <= h)
		Medianize(Lo, h, Idx, K);
	else if (K >= l)
		Medianize(l, Hi, Idx, K);

	return;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
int TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::BuildTree(int Lo, int Hi, int Idx)
{
	if (Lo > Hi)
		return -1;
	
	int ChildIdx = Idx + 1;
	if(ChildIdx == DimensionCount)
		ChildIdx = 0;
	
	Medianize(Lo, Hi, Idx, (Lo + Hi) / 2);
	Items[(Lo + Hi) / 2]->Left  = BuildTree(Lo, (Lo + Hi) / 2 - 1, ChildIdx);
	Items[(Lo + Hi) / 2]->Right = BuildTree((Lo + Hi) / 2 + 1, Hi, ChildIdx);
	return (Lo + Hi) / 2;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::DoFindInRadius(int Node, int Idx)
{
	IndexType Dist = 0;
	for(int i = 0; i < DimensionCount; i++)
		Dist += sqr(Items[Node]->Item.Index[i] - SearchCenter[i]);
	// TODO - maybe "if" inside "for" would be faster
	if(Dist <= SearchRadiusSqr)
	{
		Found[FoundCount] = &(Items[Node]->Item);
		FoundCount++;
	}

	int ChildIdx = Idx + 1;
	if(ChildIdx == DimensionCount)
		ChildIdx = 0;

	if(Items[Node]->Left != -1  && Items[Node]->Item.Index[Idx] > SearchCenter[Idx] - SearchRadius)
		DoFindInRadius(Items[Node]->Left,  ChildIdx);
	if(Items[Node]->Right != -1 && Items[Node]->Item.Index[Idx] < SearchCenter[Idx] + SearchRadius)
		DoFindInRadius(Items[Node]->Right, ChildIdx);
	return;
}

template <class ParamIndexType, int ParamDimensionCount, class ParamDataType>
inline void TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::FindInRadius
  (typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType* Center, 
	typename TKDTree<ParamIndexType, ParamDimensionCount, ParamDataType>::IndexType Radius)
{
	SearchRadius = Radius;
	SearchRadiusSqr = sqr(Radius);
	SearchCenter = Center;
	FoundCount = 0;
	DoFindInRadius(Root, 0);
	return;
}

#endif