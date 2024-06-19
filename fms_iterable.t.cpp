// fms_iterable.t.cpp - test fms_iterable.h
#include <cassert>
#include "fms_iterable.h"

using namespace fms::iterable;

inline int pointer_test()
{
	static_assert(std::contiguous_iterator<ptr<int>>);
	static_assert(!ptr<int>());
	{
		int i[] = { 1, 2, 3 };
		auto p = ptr(i, 3);
		auto p2{ p };
		assert(p == p2);
		p = p2;
		assert(!(p2 != p));

		assert(p);
		assert(*p == 1);
		++p;
		assert(p);
		assert(2 == *p);
		++p;
		assert(p);
		assert(*p == 3);
		++p;
		assert(!p);
	}
	{
		constexpr int a[] = { 1, 2, 3 };
		auto p = array(a);
		auto p2{ p };
		assert(p == p2);
		p = p2;
		assert(!(p2 != p));

		assert(p);
		assert(*p == 1);
		++p;
		assert(p);
		assert(2 == *p);
		++p;
		assert(p);
		assert(*p == 3);
		++p;
		assert(!p);
	}
	{ // compare
		int i[] = { 1, 2, 3 };
		int j[] = { 1, 2, 4 };
		assert(array(i) == array(i));
		assert(!(array(i) != array(i)));
		assert(array(i) < array(j));
		assert(array(i) <= array(j));
		assert(array(j) > array(i));
		assert(array(j) >= array(i));

		int k[] = { 1, 2 };
		auto cmp = compare(array(i), array(k));
		assert(cmp > 0);
		assert(array(i) > array(k));

		int l[] = { 1, 2, 3, 4 };
		cmp = compare(array(i), array(l));
		assert(cmp < 0);
		auto cmp2 = (array(i) < array(l));
		assert(cmp2 == cmp2);
		assert(array(i) < array(l));
	}

	return 0;
}


int main()
{
	pointer_test();

	return 0;
}