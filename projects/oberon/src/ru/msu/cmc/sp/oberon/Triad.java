package ru.msu.cmc.sp.oberon;

public class Triad<T1, T2, T3> {
	public T1 first;
	public T2 second;
	public T3 third;
	public Triad(T1 first, T2 second, T3 third) {
		this.first = first;
		this.second = second;
		this.third = third;
	}
}
