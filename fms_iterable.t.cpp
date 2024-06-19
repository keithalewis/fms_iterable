// fms_iterable.t.cpp - test fms_iterable.h
#include <cassert>
#include <list>
#include <vector>
#include "fms_iterable.h"

using namespace fms::iterable;

inline int pointer_test()
{
	static_assert(std::random_access_iterator<ptr<int>>);
	static_assert(!ptr<int>());
	{
		int i[] = { 1, 2, 3 };
		auto p = take(ptr(i), 3);
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

		auto q = array(a);
		int k = 1;
		for (auto i : q) {
			assert(i == k++);
		}
	}
	{ // compare
		int i[] = { 1, 2, 3 };
		int j[] = { 1, 2, 4 };
		assert(array(i) == array(i));
		assert(!(array(i) != array(i)));
		assert(compare(array(i), array(j)) < 0);
		assert(compare(array(i), array(j)) <= 0);
		assert(compare(array(j), array(i)) > 0);
		assert(compare(array(j), array(i)) >= 0);

		int k[] = { 1, 2 };
		assert(compare(array(i), array(k)) > 0);

		int l[] = { 1, 2, 3, 4 };
		assert(compare(array(i), array(l)) < 0);
	}
	{
		int i[] = { 1, 2, 3 };
		auto a = array(i);
		assert(a);
		assert(*a == 1);
		++a;
		assert(*a == 2);
		assert(*a++ == 2);
		assert(*a == 3);
		--a;
		assert(*a == 2);
		assert(*a-- == 2);
		assert(*a == 1);
		assert(a[0] == 1);
		assert(a[1] == 2);
		++a;
		assert(a[-1] == 1);
		assert(*(a - 1) == 1);
		assert(*(-1 + a) == 1);
	}
	{
		int i[] = { 1, 2, 3 };
		auto p = array(i);
		auto q = take(ptr(i), 2);
		assert(p != q);
		assert(p > q);
		assert(p >= q);
		assert(!(p < q));
		assert(!(p <= q));

	}

	return 0;
}

int interval_test()
{
	{
		std::vector<int> v{ 1, 2, 3 };
		std::list<int> l{ 1, 2, 3 };
		auto i = interval(v.begin(), v.end());
		auto j = make_interval(l);
		assert(compare(i, j) == 0);

		i += 2;
		// j += 2; // list not random access
		*i = 4;
		assert(v[2] == 4);

		int k = 1;
		for (auto a : j) {
			assert(a == k++);
		}
	}

	return 0;
}


int main()
{
	pointer_test();
	interval_test();

	return 0;
}