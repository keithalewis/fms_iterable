// fms_iterable.t.cpp - test fms_iterable.h
#include <cassert>
#include <list>
#include <vector>
#include "fms_iterable.h"

using namespace fms::iterable;

int drop_test()
{
	{
		int i[] = { 1, 2, 3 };
		auto a = array(i);
		assert(equal(drop(a, 0), a));
		assert(equal(drop(a, 1), take(iota(2), 2)));
		assert(equal(drop(a, 3), empty<int>()));
		//assert(equal(drop(a, 4), empty<int>()));
	}

	return 0;
}

int iota_test()
{
	{
		constexpr iota i(1);
		static_assert(i);
		constexpr iota i_{ i };
		static_assert(i == i_);
		static_assert(!(i != i_));

		static_assert(*i == 1);
		constexpr auto i2 = ++iota(i);
		static_assert(*i2 == 2);
		constexpr auto i3 = ++iota(i2);
		static_assert(*i3 == 3);
	}
	{
		constexpr iota i(1), j(2);
		static_assert(i < j);
		static_assert(j > i);
	}
	{
		iota<double> i(1);
		auto i2{ i };
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
		iota i(1);
		i = drop(i, 2);
		assert(*i == 3);
	}
	{
		assert(starts_with(iota(1), { 1,2,3 }));
		assert(starts_with(power(2), { 1,2,4 }));
		assert(starts_with(factorial(1), { 1,1,2,6 }));
		assert(starts_with(choose(3), { 1,3,3,1 }));
	}

	return 0;
}

int interval_test()
{
	//static_assert(has_end<interval<iota<int>>>);
	{
		interval i(iota(1), iota(4));
		assert(size(i) == 3);
		i = drop(i, 3);
		assert(!i);
	}
	{
		std::vector<int> v({ 1, 2, 3 });
		auto i = interval(v.begin(), v.end());
		auto i2{ i };
		assert(i == i2);
		i = i2;
		assert(!(i2 != i));

		assert(*i == 1);
		assert(*++i == 2);
		assert(*i++ == 2);
		assert(*i == 3);
		++i;
		assert(!i);

		--i;
		assert(*i == 3);
		i -= 2;
		assert(*i == 1);
		//i[1] = 4;
		//assert(*(i + 1) == 4);
	}
	{
		std::vector<int> v({ 1, 2, 3 });
		std::list<int> l({ 1, 2, 3 });
		auto i = interval(v.begin(), v.end());
		auto j = make_interval(l);
		assert(equal(i, j));

		i[1] = 4;
		copy(i, j);
		assert(*++j == 4);
		assert(*back(j) == 3);

		std::vector<int> w;
		copy(i, back_insert_iterable(w));
		assert(equal(i, make_interval(w)));
	}
	{
		auto i = interval(iota(1), iota(4));
		assert(equal(i, { 1,2,3 }));
		i = drop(i, 1);
		assert(equal(i, { 2,3 }));
		i = drop(i, 2);
		assert(!i);
	}

	return 0;
}

int ptr_test()
{
	static_assert(std::random_access_iterator<ptr<int>>);
	static_assert(!ptr<int>());
	static_assert(size(empty<int>()) == 0);
	{
		int i[] = { 1, 2, 3 };
		auto p = interval(ptr(i), ptr(i + 3));
		assert(size(p) == 3);
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
		int a[] = { 1, 2, 3 };
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
		auto q = counted(ptr(i), 2);
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

int counted_test()
{
	//static_assert(std::random_access_iterator<counted<ptr<int>>>);
	{
		auto c = counted(iota(1), 3);
		auto c2{ c };
		assert(c == c2);
		c = c2;
		assert(!(c2 != c));

		assert(*c == 1);
		++c;
		assert(*c == 2);
		assert(*c++ == 2);
		assert(*c == 3);
		++c;
		assert(!c);
	}

	return 0;
}

int repeat_test()
{
	{
		assert(equal(counted(constant(1), 3), counted(repeat(single(1)), 3)));
	}

	return 0;
}

int constant_test()
{
	static_assert(std::random_access_iterator<constant<int>>);
	{
		constant c(1);
		auto c2{ c };
		assert(c2 == c);
		c = c2;
		assert(!(c2 != c));
	}

	return 0;
}

int concatenate_test()
{
	{
		const auto i = concatenate(counted(iota(1), 3), counted(iota(4), 3));
		assert(equal(i, counted(iota(1), 6)));
		assert(equal(i, concatenate(i, empty<int>())));
		assert(equal(concatenate(i, empty<int>()), i));
	}

	return 0;
}

int merge_test()
{
	{
		const auto i = merge(counted(iota(1), 3), counted(iota(2), 3));
		assert(equal(i, { 1,2,2,3,3,4 }));
		assert(equal(i, merge(i, empty<int>())));
		assert(equal(merge(i, empty<int>()), i));
	}
	{
		const auto i = merge(counted(iota(4), 3), counted(iota(1), 3));
		assert(equal(i, { 1,2,3,4,5,6 }));
	}
	{
		const auto i = merge(counted(iota(3), 3), counted(iota(1), 3));
		assert(equal(i, { 1,2,3,3,4,5 }));
	}

	return 0;
}

bool is_even(int i) {
	return i % 2 == 0;
}

int apply_test()
{
	{
		auto i = apply([](int i) { return i + 1; }, counted(iota(1), 3));
		auto i2{ i };
		assert(i == i2);
		i = i2;
		assert(!(i2 != i));

		assert(equal(i, { 2,3,4 }));
	}
	{
		auto i = apply(is_even, counted(iota(1), 3));
		auto i2{ i };
		assert(i == i2);
		i = i2;
		assert(!(i2 != i));

		assert(equal(i, { false,true,false }));
	}

	return 0;
}

int filter_test()
{
	{
		auto f = filter(is_even, counted(iota(1), 6));
		auto f2{ f };
		assert(f == f2);
		f = f2;
		assert(!(f2 != f));

		assert(f);
		assert(*f == 2);
		++f;
		assert(*f == 4);
		assert(*f++ == 4);
		assert(*f == 6);
		assert(!++f);
	}

	return 0;
}

int until_test()
{
	{
		const auto eps = [](double x) { return 1 + x == 1; };
		auto u = until(eps, power(0.5));
		auto u2{ u };
		assert(u == u2);
		u = u2;
		assert(!(u2 != u));

		auto n = size(u);
		assert(n == std::numeric_limits<double>::digits);
	}

	return 0;
}

int fold_test()
{
	{
		auto f = fold(std::plus<int>{}, constant(1));
		assert(equal(counted(f, 3), counted(iota(0), 3)));
		assert(sum(counted(iota(1), 4)) == 1 + 2 + 3 + 4);
		assert(prod(counted(iota(1), 4)) == 1 * 2 * 3 * 4);
	}

	return 0;
}

int delta_test()
{
	{
		auto d = delta(take(iota(0), 3));
		auto d2{ d };
		assert(d == d2);
		d = d2;
		assert(!(d2 != d));

		assert(equal(d, { 1, 1 }));
	}
	{
		auto d = delta(apply([](int i) { return i * i; }, take(iota(1), 3)));
		assert(equal(d, { 4 - 1, 9 - 4 }));
	}

	return 0;
}

int main()
{
	drop_test();
	iota_test();
	interval_test();
	ptr_test();
	counted_test();
	repeat_test();
	constant_test();
	concatenate_test();
	merge_test();
	apply_test();
	filter_test();
	until_test();
	fold_test();
	delta_test();

	return 0;
}
