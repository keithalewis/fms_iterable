// fms_iterable.t.cpp - test fms_iterable.h
#include <cassert>
#include <list>
#include <vector>
#include "fms_iterable.h"

using namespace fms::iterable;

template<class I>
inline std::vector<typename I::value_type> vector(I i)
{
	std::vector<typename I::value_type> v;

	for (auto j : i) {
		v.push_back(j);
	}

	return v;
}

int drop_test()
{
	{
		int i[] = { 1, 2, 3 };
		auto a = array(i);
		assert(equal(drop(a, 0), a));
		assert(equal(drop(a, 1), take(iota(2), 2)));
		assert(equal(drop(a, 3), empty<int>()));
		assert(equal(drop(a, 4), empty<int>()));
	}

	return 0;
}

int iota_test()
{
	{
		iota i(1);
		assert(i);
		iota i2{ i };
		assert(i == i2);
		i = i2;
		assert(!(i2 != i));

		assert(*i == 1);
		++i;
		assert(*i == 2);
		assert(*i++ == 2);
		assert(*i == 3);
	}
	{
		assert(vector(take(iota(1), 3)) == std::vector({ 1,2,3 }));
		assert(vector(take(power(2), 3)) == std::vector({ 1,2,4 }));
		assert(vector(take(factorial(1), 4)) == std::vector({ 1,1,2,6 }));
		assert(vector(choose(3)) == std::vector({ 1,3,3,1 }));
	}

	return 0;
}

int pointer_test()
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
		assert(compare(array(i), array(i)) == 0);
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
		// p and q have pointers to the same array
		assert(p != q);
		assert(p > q);
		assert(p >= q);
		assert(!(p < q));
		assert(!(p <= q));

	}
	{
		int i[] = { 1, 2, 3 };
		auto a = array(i);
		assert(size(a) == 3);
		//auto b = back(a);
		assert(*back(a) == 3);
		assert(*--end(a) == 3);
		a[1] = 4;
		assert(*(a + 1) == 4);
	}

	return 0;
}

int interval_test()
{
	static_assert(has_end<interval<ptr<int>>>);
	{
		std::vector<int> v{ 1, 2, 3 };
		std::list<int> l{ 1, 2, 3 };
		auto i = interval(v.begin(), v.end());
		auto j = make_interval(l);
		assert(equal(i, j));
		assert(compare(take(i, 2), take(j, 2)) == 0);
		assert(compare(take(i, 3), take(j, 2)) > 0);
		assert(compare(take(i, 2), take(j, 3)) < 0);

		i += 2;
		// j += 2; // list not random access
		*i = 4;
		assert(v[2] == 4);

		int k = 1;
		for (auto a : j) {
			assert(a == k++);
		}
		++j;
		*j = 5;
		assert(*j == 5);
	}

	return 0;
}

int repeat_test()
{
	{
		assert(equal(take(constant(1), 3), take(repeat(once(1)), 3)));
	}

	return 0;
}

int concatenate_test()
{
	{
		const auto i = concatenate(take(iota(1), 3), take(iota(4), 3));
		assert(equal(i, take(iota(1), 6)));
		assert(equal(i, concatenate(i, empty<int>())));
		assert(equal(concatenate(i, empty<int>()), i));
	}

	return 0;
}

int merge_test()
{
	{
		const auto i = merge(take(iota(1), 3), take(iota(2), 3));
		assert(vector(i) == std::vector({ 1,2,2,3,3,4 }));
		assert(equal(i, merge(i, empty<int>())));
		assert(equal(merge(i, empty<int>()), i));
	}
	{
		const auto i = merge(take(iota(4), 3), take(iota(1), 3));
		assert(vector(i) == std::vector({ 1,2,3,4,5,6 }));
	}
	{
		const auto i = merge(take(iota(3), 3), take(iota(1), 3));
		assert(vector(i) == std::vector({ 1,2,3,3,4,5 }));
	}

	return 0;
}

int apply_test()
{
	{
		auto i = apply([](int i) { return i + 1; }, take(iota(1), 3));
		auto i2{ i };
		assert(i == i2);
		i = i2;
		assert(!(i2 != i));

		assert(vector(i) == std::vector({ 2,3,4 }));
	}

	return 0;
}

int main()
{
	drop_test();
	iota_test();
	pointer_test();
	interval_test();
	repeat_test();
	concatenate_test();
	merge_test();
	apply_test();

	return 0;
}
