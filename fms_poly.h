// fms_poly.h - polynomials
#pragma once
#include <iterator>

namespace fms::poly {

	template<class I>
	class counted {
		I i;
		size_t n;
	public:
		using iterator_category = typename I::iterator_category;
		using value_type = std::iter_value_t<I>;

		constexpr counted(I i, size_t n)
			: i(i), n(n)
		{ }

		constexpr operator bool() const
		{
			return n;
		}
		constexpr value_type operator*() const
		{
			return *i;
		}
		constexpr counted& operator++()
		{
			if (n) {
				++i;
				--n;
			}

			return *this;
		}
		constexpr counted operator++(int)
		{
			auto tmp{ *this };
		
			operator++();
			
			return tmp;
		}
	};
	template<class T, size_t N>
	constexpr auto array(T(&a)[N])
	{
		return counted(a, N);
	}
#ifdef _DEBUG
	namespace {
		namespace {
			constexpr int a[] = { 1, 2 };
			constexpr auto ca = counted(a, 2);
			static_assert(ca);
			static_assert(*ca == 1);
			static_assert(*++counted(a, 2) == 2);
			static_assert(!++counted(a, 1));
		}
		namespace {
			constexpr int a[] = { 1, 2 };
			constexpr auto ca = array(a);
			static_assert(ca);
			static_assert(*ca == 1);
			static_assert(*++array(a) == 2);
		}
	}
#endif // _DEBUG

	// Value of polynomial at x with coefficients I.
	template<class I, class T = std::iter_value_t<I>>
	constexpr T value(I i, T x)
	{
		if constexpr (i) {
			return *i + x * value(++i, x);
		}

		return T(0);
	}

	template<class I, class T = std::iter_value_t<I>>
	constexpr T bspline(size_t p, I i, T x)
	{
		while (i && *i < x) {
			++i;
		}
		if (p == 0) {
			return *i <= x && x < *++i ? T(1) : T(0);
		}

		T d = *i;
		while (i && *i <= x) {
			++i;
		}
		== *++i ? T(0) : (x - *i) / (*++i - *i);

		return d * bspline(p - 1, i, x) + (1 - d) * bspline(p - 1, ++i, x);
	}
	// https://www.sciencedirect.com/science/article/pii/002190459190006V?via%3Dihub
	// int g(t) M(t|x0,...,xk) dt = int_Ek g(sum_j u_j x_j) dmu(u)
	// E_k = {(u0,...,uk) | sum_j u_j = 1, u_j >= 0}
	// dmu(u) = k! du1...duk
	// g(t) = exp(st)

} // namespace fms::poly
