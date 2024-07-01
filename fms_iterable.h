// fms_iterable.h - iterators with operator bool() sentinel
#pragma once
#include <compare>
#include <concepts>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <optional>
#include <tuple>
#include <utility>

namespace fms::iterable {

	template <class I>
	concept has_op_bool = requires(I i) {
		{ i.operator bool() } -> std::same_as<bool>;
	};

	template<class I> concept input_iterable
		= std::input_iterator<I> && has_op_bool<I>;
	template<class I, class T> concept output_iterable
		= std::output_iterator<I, T>&& has_op_bool<I>;
	template<class I> concept forward_iterable
		= std::forward_iterator<I> && has_op_bool<I>;
	template<class I> concept bidirectional_iterable
		= std::bidirectional_iterator<I> && has_op_bool<I>;
	template<class I> concept random_access_iterable
		= std::random_access_iterator<I> && has_op_bool<I>;
	template<class I> concept contiguous_iterable
		= std::contiguous_iterator<I> && has_op_bool<I>;

	template<class C>
	struct back_insert_iterable : public std::back_insert_iterator<C>
	{
		constexpr explicit back_insert_iterable(C& c) noexcept
			: std::back_insert_iterator<C>(c)
		{ }
		constexpr explicit operator bool() const noexcept
		{
			return true;
		}
	};
	template <class C>
	[[nodiscard]] constexpr back_insert_iterable<C> back_inserter(C& c) noexcept
	{
		return back_insert_iterable<C>(c);
	}

	template<class C>
	struct front_insert_iterable : public std::front_insert_iterator<C>
	{
		constexpr explicit front_insert_iterable(C& c) noexcept
			: std::front_insert_iterator<C>(c)
		{ }
		constexpr explicit operator bool() const noexcept
		{
			return true;
		}
	};
	template <class C>
	[[nodiscard]] constexpr front_insert_iterable<C> front_inserter(C& c) noexcept
	{
		return front_insert_iterable<C>(c);
	}

	template <class I>
	concept has_begin = requires(I i) {
		{ i.begin() } -> std::same_as<I>;
	};
	template <class I>
	concept has_end = requires(I i) {
		{ i.end() } -> std::same_as<I>;
	};
	template <class I>
	concept has_last = requires(I i) {
		{ i.last() } -> std::same_as<I>;
	};
	template <class I>
	concept has_size = requires(I i) {
		{ i.size() } -> std::same_as<std::size_t>;
	};

	// Lexicographically compare at most n elements of two iterables
	template<class I, class J>
	constexpr auto compare(I i, J j)
		requires has_end<I> && has_end<J>
	{
		while (i && j) {
			const auto cmp = *i++ <=> *j++;
			if (cmp != 0) {
				return cmp;
			}
		}

		return !!i <=> !!j;
	}
	// All elements are equal.
	template<class I, class J>
		requires has_end<I>&& has_end<J>
	constexpr auto equal(I i, J j)
	{
		return compare(i, j) == 0;
	}

	// i equals {t,...}
	template<class I, class T = std::iter_value_t<I>>
	constexpr bool equal(I i, std::initializer_list<T> ts)
	{
		for (const auto& t : ts) {
			if (!i || *i++ != t) {
				return false;
			}
		}

		return !i;
	}
	// i starts with {t,...}
	template<class I, class T = std::iter_value_t<I>>
	constexpr bool starts_with(I i, std::initializer_list<T> ts)
	{
		for (const auto& t : ts) {
			if (!i || *i++ != t) {
				return false;
			}
		}

		return true;
	}

	template<class I, class J>
	constexpr auto copy(I i, J j)
	{
		while (i && j) {
			*j++ = *i++;
		}

		return j;
	}
	template<class I, class J>
	constexpr auto copy_n(I i, J j, std::size_t n)
	{
		while (n-- && i && j) {
			*j++ = *i++;
		}

		return j;
	}

	// Last element of iterable. ++last(i) == end(i)
	template <class I>
	constexpr I last(I i)
	{
		if constexpr (has_last<I>) {
			return i.last();
		}
		else if constexpr (has_end<I> && std::bidirectional_iterator<I>) {
			return --i.end();
		}
		else {
			I _i(i);
			while (i) {
				_i = i;
				++i;
			}
			return _i;
		}
	}

	// For use with STL
	template <class I>
	constexpr I begin(I i)
	{
		return i;
	}
	// ++last(i)
	template <class I>
	constexpr I end(I i)
	{
		if constexpr (has_end<I>) {
			return i.end();
		}
		else {
			while (i.operator bool()) {
				++i;
			}
			return i;
		}
	}

	// size(i, size(j)) = size(i) + size(j)
	template <class I>
	constexpr std::iter_difference_t<I> size(I i, std::iter_difference_t<I> n = 0) noexcept
	{
		if constexpr (has_end<I>) {
			return n + std::distance(i, i.end());
		}
		else {
			while (i.operator bool()) {
				++i;
				++n;
			}

			return n;
		}
	}

	// Drop at most n from the beginning.
	template <class I>
	constexpr I drop(I i, std::iter_difference_t<I> n) noexcept
	{
		if constexpr (has_end<I>) {
			return std::next(i, std::min(n, size(i)));
		}
		else {
			if (n > 0) {
				while (n-- && i) {
					++i;
				}
			}
			else if (n < 0) {
				while (n++ && i) {
					--i;
				}
			}

			return i;
		}
	}

	// Iterable over [b, e)
	template<class I>
	class interval : public I {
		I e;
	public:
		constexpr interval(I i, I e)
			: I(i), e(e)
		{ }
		constexpr interval(const interval& i) = default;
		constexpr interval& operator=(const interval& i) = default;
		constexpr interval(interval&&) = default;
		constexpr interval& operator=(interval&&) = default;
		constexpr ~interval() = default;

		constexpr auto operator<=>(const interval& i) const = default;

		constexpr interval begin() const
		{
			return *this;
		}
		constexpr interval end() const
		{
			return interval(e, e);
		}

		constexpr explicit operator bool() const noexcept
		{
			return *this != e;
		}
	};

	// Assumes lifetime of container.
	template<class C>
	constexpr auto make_interval(C& c)
	{
		return interval(c.begin(), c.end());
	}

	// t, t + 1, ...
	template<class T>
	class iota {
		T t;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr iota(T t = 0)
			: t(t)
		{ }
		constexpr virtual ~iota() = default;

		constexpr auto operator<=>(const iota&) const = default;

		constexpr auto begin() const
		{
			return *this;
		}
		// no end()

		constexpr virtual explicit operator bool() const noexcept
		{
			return true;
		}
		constexpr value_type operator*() const noexcept
		{
			return t;
		}
		constexpr iota& operator++() noexcept
		{
			++t;

			return *this;
		}
		constexpr iota operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
		// bidirectional
		constexpr iota& operator--() noexcept
		{
			--t;

			return *this;
		}
		constexpr iota operator--(int) noexcept
		{
			auto tmp{ *this };

			operator--();

			return tmp;
		}
		// random access???
	};

	// tn, tn*t, tn*t*t, ...
	template <class T>
	class power {
		T t, tn;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr power(T t, T tn = 1)
			: t(t), tn(tn)
		{ }
		constexpr virtual ~power() = default;

		constexpr bool operator==(const power& p) const = default;

		constexpr auto begin() const
		{ 
			return *this; 
		}
		// no end()

		constexpr virtual explicit operator bool() const noexcept
		{
			return true;
		}
		constexpr value_type operator*() const noexcept
		{
			return tn;
		}
		constexpr power& operator++() noexcept
		{
			tn *= t;

			return *this;
		}
		constexpr power operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
		// bidirectional
		constexpr power& operator--() noexcept
		{
			tn /= t;

			return *this;
		}
		constexpr power operator--(int) noexcept
		{
			auto tmp{ *this };

			operator--();

			return tmp;
		}
		// random access???
	};

	// 1, 1, 1*2, 1*2*3, ...
	template <class T = double>
	class factorial {
		T t, n;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr factorial(T t = 1)
			: t(t), n(1)
		{ }
		constexpr virtual ~factorial() = default;

		constexpr bool operator==(const factorial& f) const = default;

		constexpr auto begin() const
		{
			return *this;
		}
		// no end()

		constexpr virtual explicit operator bool() const noexcept
		{
			return true;
		}
		constexpr value_type operator*() const noexcept
		{
			return t;
		}
		constexpr factorial& operator++() noexcept
		{
			t *= n++;

			return *this;
		}
		constexpr factorial operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// 1, n, n*(n-1)/2, ..., 1
	template <class T = std::size_t>
	class choose {
		T n, k, nk;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr choose(T n)
			: n(n), k(0), nk(1)
		{ }
		constexpr virtual ~choose() = default;

		constexpr bool operator==(const choose& c) const = default;

		constexpr choose begin() const
		{
			return choose(n);
		}
		constexpr choose end() const
		{
			return choose{ n, n + 1, 0 };
		}

		constexpr virtual explicit operator bool() const noexcept
		{
			return k <= n;
		}
		value_type operator*() const noexcept
		{
			return nk;
		}
		choose& operator++() noexcept
		{
			if (operator bool()) {
				nk *= n - k;
				++k;
				nk /= k;
			}

			return *this;
		}
		choose operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// Unsafe pointer iterable with std::span/view semantics.
	template<class T>
	class ptr {
	protected:
		T* p;
	public:
		using iterator_category = std::random_access_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr ptr(T* p = nullptr)
			: p(p)
		{ }
		constexpr ptr(const ptr&) = default;
		constexpr ptr& operator=(const ptr&) = default;
		constexpr ptr(ptr&&) = default;
		constexpr ptr& operator=(ptr&&) = default;
		constexpr virtual ~ptr() = default;

		constexpr auto operator<=>(const ptr& _p) const = default;

		constexpr ptr begin() const
		{
			return *this;
		}
		// no end()

		constexpr virtual explicit operator bool() const noexcept
		{
			return p != nullptr;
		}
		//??? why not value_type
		// indirectly readable
		constexpr reference operator*() const noexcept
		{
			return *p;
		}
		// indirectly writable
		constexpr reference operator*() noexcept
		{
			return *p;
		}
		// weakly incrementable
		constexpr ptr& operator++() noexcept
		{
			++p;

			return *this;
		}
		constexpr ptr operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
		// bidirectional
		constexpr ptr& operator--() noexcept
		{
			--p;

			return *this;
		}
		constexpr ptr operator--(int) noexcept
		{
			auto tmp{ *this };

			operator--();

			return tmp;
		}
		// random access
		constexpr ptr& operator+=(difference_type d) noexcept
		{
			p += d;

			return *this;
		}
		constexpr ptr operator+(difference_type d) const noexcept
		{
			return ptr(p + d);
		}
		constexpr friend ptr<T> operator+(difference_type d, ptr<T> p) noexcept
		{
			return p + d;
		}
		constexpr ptr& operator-=(difference_type d) noexcept
		{
			p -= d;

			return *this;
		}
		constexpr ptr operator-(difference_type d) const noexcept
		{
			return ptr(p - d);
		}
		constexpr difference_type operator-(const ptr& i) const noexcept
		{
			return p - i.p;
		}
		// TODO: value_type
		reference operator[](difference_type i) const noexcept
		{
			return p[i];
		}
		reference operator[](difference_type i) noexcept
		{
			return p[i];
		}
	};

	// Iterable over [i, i + n).
	template<class I>
	class counted : public I {
		std::size_t n;
	public:
		using iterator_category = typename I::iterator_category;
		using value_type = typename I::value_type;
		using reference = typename I::reference;
		using pointer = typename I::pointer;
		using difference_type = typename I::difference_type;

		constexpr counted() = default;
		constexpr counted(I i, std::size_t n)
			: I(i), n(n)
		{ }
		constexpr counted(const counted&) = default;
		constexpr counted& operator=(const counted&) = default;
		constexpr counted(counted&&) = default;
		constexpr counted& operator=(counted&&) = default;
		constexpr ~counted() = default;

		/*constexpr*/ auto operator<=>(const counted& i) const = default;

		constexpr counted begin() const
		{
			return counted(I::begin(), n);
		}
		constexpr counted end() const
		{
			return counted(drop(I::begin(), n), 0);
		}

		constexpr explicit operator bool() const noexcept
		{
			return n != 0;
		}
		// indirectly readable
		constexpr value_type operator*() const noexcept
		{
			return I::operator*();
		}
		// indirectly writable
		constexpr reference operator*() noexcept
			requires std::indirectly_writable<I, value_type>
		{
			return I::operator*();
		}
		// weakly incrementable
		constexpr counted& operator++() noexcept
		{
			if (operator bool()) {
				I::operator++();
				--n;
			}

			return *this;
		}
		constexpr counted operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
		// bidirectional
		constexpr counted& operator--() noexcept
			requires std::bidirectional_iterator<I>
		{
			I::operator--();
			++n;

			return *this;
		}
		constexpr counted operator--(int) noexcept
			requires std::bidirectional_iterator<I>
		{
			auto tmp{ *this };

			operator--();

			return tmp;
		}
		// random access
		constexpr counted& operator+=(difference_type d) noexcept
			requires std::random_access_iterator<I>
		{
			I::operator+=(d);
			n -= d;

			return *this;
		}
		constexpr counted operator+(difference_type d) const noexcept
			requires std::random_access_iterator<I>
		{
			return counted(I::operator+(d), n - d);
		}
		constexpr friend counted<I> operator+(std::ptrdiff_t d, counted<I> c) noexcept
			requires std::random_access_iterator<I>
		{
			return c += d;
		}
		constexpr counted& operator-=(difference_type d) noexcept
			requires std::random_access_iterator<I>
		{
			I::operator-=(d);
			n += d;

			return *this;
		}
		constexpr difference_type operator-(const counted& _i) const noexcept
			requires std::random_access_iterator<I>
		{
			return I::operator-(_i);
		}
		constexpr counted operator-(difference_type d) const noexcept
			requires std::random_access_iterator<I>
		{
			return counted(I::operator-(d), n + d);
		}
		constexpr reference operator[](difference_type d) const noexcept
			requires std::random_access_iterator<I>
		{
			return I::operator[](d);
		}
		constexpr reference operator[](difference_type d) noexcept
			requires std::random_access_iterator<I>
		{
			return I::operator[](d);
		}
	};

	// Assumes lifetime of a.
	template<class T, std::size_t N>
	constexpr auto array(T(&a)[N]) noexcept
	{
		return counted(ptr(a), N);
	}

	// Take at most n elements from i.
	template<class I>
	constexpr auto take(I i, std::iter_difference_t<I> n)
	{
		if constexpr (has_end<I>) {
			n = std::min(n, size(i));
		}

		return counted(i, n);
	}
	// Iterable with no elements.
	template<class T>
	constexpr auto empty()
	{
		return take(ptr<T>(), 0);
	}


	// Cycle over iterator values.
	template<class I>
	class repeat {
		I i, i0;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = typename I::value_type;
		using reference = typename I::reference;
		using pointer = typename I::pointer;
		using difference_type = typename I::difference_type;

		constexpr repeat() = default;
		constexpr repeat(I i)
			: i(i), i0(i)
		{ }
		constexpr repeat(const repeat&) = default;
		constexpr repeat& operator=(const repeat&) = default;
		constexpr repeat(repeat&&) = default;
		constexpr repeat& operator=(repeat&&) = default;
		constexpr ~repeat() = default;

		constexpr auto operator<=>(const repeat& c) const = default;

		constexpr repeat begin() const
		{
			return *this;
		}
		// no end()

		constexpr explicit operator bool() const noexcept
		{
			return i.operator bool(); // repeat(empty) = empty
		}
		constexpr value_type operator*() const noexcept
		{
			return *i;
		}
		constexpr reference operator*() noexcept
			requires std::indirectly_writable<I, value_type>
		{
			return *i;
		}
		constexpr repeat& operator++() noexcept
		{
			if (!++i) {
				i = i0;
			}

			return *this;
		}
		constexpr repeat operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	template<class I>
	constexpr auto rotate(I i, std::iter_difference_t<I> n)
	{
		return take(drop(repeat(i), n), size(i));
	}

	template<class T>
	class constant {
		T t;
	public:
		using iterator_category = std::random_access_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr constant() = default;
		constexpr constant(T t)
			: t(t)
		{ }
		constexpr constant(const constant&) = default;
		constexpr constant& operator=(const constant&) = default;
		constexpr constant(constant&&) = default;
		constexpr constant& operator=(constant&&) = default;
		constexpr ~constant() = default;

		constexpr auto operator<=>(const constant& c) const
		{
			return this <=> &c;
		}

		constexpr constant begin() const
		{
			return *this;
		}
		// no end()

		constexpr operator bool() const
		{
			return true;
		}
		constexpr value_type operator*() const
		{
			return t;
		}
		constexpr constant& operator++()
		{
			return *this;
		}
		constexpr constant operator++(int)
		{
			return *this;
		}
		// bidirectional
		constexpr constant& operator--() noexcept
		{
			return *this;
		}
		constexpr constant operator--(int) noexcept
		{
			return *this;
		}
		// random access
		constexpr constant& operator+=(difference_type)
		{
			return *this;
		}
		constexpr constant operator+(difference_type) const
		{
			return *this;
		}
		constexpr friend constant operator+(difference_type d, const constant& c)
		{
			return c;
		}
		constexpr constant& operator-=(difference_type)
		{
			return *this;
		}
		constexpr constant operator-(difference_type) const
		{
			return *this;
		}
		constexpr difference_type operator-(const constant&) const
		{
			return 0;
		}
		constexpr value_type operator[](difference_type) const
		{
			return t;
		}
		constexpr reference operator[](difference_type)
		{
			return t;
		}
	};

	template<class T>
	constexpr auto single(T t)
	{
		return take(constant<T>(t), 1);
	}

	// i0 then i1
	template <class I0, class I1, class T = std::common_type_t<typename I0::value_type, typename I1::value_type>>
	class concatenate2 {
		I0 i0;
		I1 i1;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::common_type_t<typename I0::difference_type, typename I1::difference_type>;

		constexpr concatenate2() = default;
		constexpr concatenate2(const I0& i0, const I1& i1)
			: i0(i0), i1(i1)
		{ }
		constexpr concatenate2(const concatenate2&) = default;
		constexpr concatenate2& operator=(const concatenate2&) = default;
		constexpr concatenate2(concatenate2&&) = default;
		constexpr concatenate2& operator=(concatenate2&&) = default;
		constexpr ~concatenate2() = default;

		constexpr bool operator==(const concatenate2& i) const = default;

		constexpr auto begin() const
		{
			return *this;
		}
		constexpr auto end() const
		{
			return concatenate2(i0.end(), i1.end());
		}

		constexpr explicit operator bool() const
		{
			return i0 || i1;
		}
		constexpr value_type operator*() const
		{
			return i0 ? *i0 : *i1;
		}
		constexpr concatenate2& operator++()
		{
			if (i0) {
				++i0;
			}
			else {
				++i1;
			}

			return *this;
		}
		constexpr concatenate2 operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};
	template<class I>
	constexpr auto concatenate(I i)
	{
		return i;
	}
	template<class I, class ...Is>
	constexpr auto concatenate(I i, Is... is)
	{
		return concatenate2(i, concatenate(is...));
	}

	// Sorted i0 and i1 in order. Equivalent (!< and !>) elements are repeated.
	template <class I0, class I1, class T = std::common_type_t<typename I0::value_type, typename I1::value_type>>
	class merge2 {
		I0 i0;
		I1 i1;
		bool _0; // true use i0, false use i1
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::common_type_t<typename I0::difference_type, typename I0::difference_type>;

		constexpr merge2() = default;
		constexpr merge2(const I0& i0, const I1& i1)
			: i0(i0), i1(i1)
		{
			if (i0 && i1) {
				if (*i1 < *i0) {
					_0 = false;
				}
				else { // less or equivalent
					_0 = true;
				}
			}
			else if (i0) {
				_0 = true;
			}
			else {
				_0 = false;
			}
		}
		constexpr merge2(const merge2&) = default;
		constexpr merge2& operator=(const merge2&) = default;
		constexpr merge2(merge2&&) = default;
		constexpr merge2& operator=(merge2&&) = default;
		constexpr ~merge2() = default;

		constexpr bool operator==(const merge2& i) const = default;

		constexpr auto begin() const
		{
			return *this;
		}
		constexpr auto end() const
		{
			return merge2(i0.end(), i1.end());
		}

		constexpr explicit operator bool() const
		{
			return i0 || i1;
		}
		constexpr value_type operator*() const
		{
			if (i0 && i1) {
				if (*i0 < *i1) {
					return *i0;
				}
				else if (*i1 < *i0) {
					return *i1;
				}
				else {
					return _0 ? *i0 : *i1;
				}
			}

			return i0 ? *i0 : *i1;
		}
		constexpr merge2& operator++()
		{
			if (i0 && i1) {
				if (*i0 < *i1) {
					++i0;
				}
				else if (*i1 < *i0) {
					++i1;
				}
				else { // equivalent
					if (_0) {
						++i0;
					}
					else {
						++i1;
					}
					_0 = !_0; // switch
				}
			}
			else {
				if (i0) {
					++i0;
					_0 = true;
				}
				else if (i1) {
					++i1;
					_0 = false;
				}
			}


			return *this;
		}
		constexpr merge2 operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};
	template<class I>
	constexpr auto merge(I i)
	{
		return i;
	}
	template<class I, class ...Is>
	constexpr auto merge(I i, Is... is)
	{
		return merge2(i, merge(is...));
	}

	// copy assignable function object
	template<class F>
	class copy_assignable {
		std::optional<F> f;
	public:
		constexpr copy_assignable() noexcept = default;
		constexpr copy_assignable(F f) noexcept
			: f(std::move(f))
		{ }
		copy_assignable(const copy_assignable& a)
		{
			f.reset();
			if (a.f.has_value()) {
				f.emplace(a.f.value());
			}
		}
		copy_assignable& operator=(const copy_assignable& a)
		{
			if (this != &a) {
				f.reset();
				if (a.f.has_value()) {
					f.emplace(a.f.value());
				}
			}

			return *this;
		}
		copy_assignable(copy_assignable&&) = default;
		copy_assignable& operator=(copy_assignable&&) = default;
		~copy_assignable() = default;

		template<class... Args>
		constexpr auto operator()(Args&&... args) const
		{
			return f.has_value() ? f.value()(std::forward<Args>(args)...) : decltype(f.value()(std::forward<Args>(args)...)){};
		}
	};

	// Apply a function to elements of an iterable.
	template <class F, class I>
	class apply {
		using T = std::iter_value_t<I>;
		using U = std::invoke_result_t<F, T>;

		copy_assignable<F> f; // for operator=(const apply&)
		I i;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = U;
		using reference = U&;
		using pointer = U*;
		using difference_type = typename I::difference_type;

		constexpr apply() = default;
		constexpr apply(F f, const I& i)
			: f(std::move(f)), i(i)
		{ }
		constexpr apply(const apply& a) = default;
		constexpr apply& operator=(const apply& a) = default;
		constexpr apply(apply&& a) = default;
		constexpr apply& operator=(apply&& a) = default;
		constexpr ~apply() = default;

		constexpr bool operator==(const apply& a) const
		{
			return i == a.i; // F is part of type
		}

		constexpr explicit operator bool() const
		{
			return i.operator bool();
		}
		constexpr value_type operator*() const
		{
			return f(*i);
		}
		constexpr apply& operator++() noexcept
		{
			++i;

			return *this;
		}
		constexpr apply operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// Apply a binary operation to elements of two iterable.
	template <class BinOp, class I0, class I1>
	class binop {
		using T0 = std::iter_value_t<I0>;
		using T1 = std::iter_value_t<I1>;
		using T = std::invoke_result_t<BinOp, T0, T1>;
		
		copy_assignable<BinOp> op;
		I0 i0;
		I1 i1;
		public:
			using iterator_category = std::input_iterator_tag;
			using value_type = T;
			using reference = T&;
			using difference_type = std::ptrdiff_t;

			constexpr binop(BinOp op, I0 i0, I1 i1)
				: op(std::move(op)), i0(i0), i1(i1)
			{ }
			constexpr binop(const binop& o) = default;
			constexpr binop& operator=(const binop& o) = default;
			constexpr binop(binop&& o) noexcept = default;
			constexpr binop& operator=(binop&& o) = default;
			constexpr ~binop() = default;

			constexpr bool operator==(const binop& o) const
			{
				return i0 == o.i0 && i1 == o.i1;
			}

			constexpr explicit operator bool() const
			{
				return i0 && i1;
			}
			constexpr value_type operator*() const
			{
				return op(*i0, *i1);
			}
			constexpr binop& operator++() noexcept
			{
				++i0;
				++i1;

				return *this;
			}
			constexpr binop operator++(int) noexcept
			{
				auto b{ *this };

				operator++();

				return b;
			}
	};


	// Elements satisfying predicate.
	template <class P, class I>
	class filter {
		using T = std::iter_value_t<I>;

		copy_assignable<P> p;
		I i;

		constexpr void next()
		{
			while (i && !p(*i)) {
				++i;
			}
		}
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = typename I::difference_type;

		constexpr filter() = default;
		constexpr filter(P p, const I& i)
			: p(std::move(p)), i(i)
		{
			next();
		}
		constexpr filter(const filter& a) = default;
		constexpr filter& operator=(const filter& a) = default;
		constexpr filter(filter&& a) = default;
		constexpr filter& operator=(filter&& a) = default;
		constexpr ~filter() = default;

		constexpr bool operator==(const filter& a) const
		{
			return i == a.i; // P is part of type
		}

		constexpr explicit operator bool() const
		{
			return i.operator bool();
		}
		constexpr value_type operator*() const
		{
			return *i;
		}
		constexpr filter& operator++() noexcept
		{
			++i;
			next();

			return *this;
		}
		constexpr filter operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// Stop at first element satisfying predicate.
	template <class P, class I, class T = typename I::value_type>
	class until {
		copy_assignable<P> p;
		I i;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = typename I::difference_type;

		constexpr until() = default;
		constexpr until(P p, const I& i)
			: p(std::move(p)), i(i)
		{ }
		constexpr until(const until&) = default;
		constexpr until& operator=(const until&) = default;
		constexpr until(until&&) = default;
		constexpr until& operator=(until&&) = default;
		constexpr ~until() = default;

		constexpr bool operator==(const until& u) const
		{
			return i == u.i;
		}

		constexpr explicit operator bool() const
		{
			return i && !p(*i);
		}
		constexpr value_type operator*() const
		{
			return *i;
		}
		constexpr until& operator++() noexcept
		{
			++i;

			return *this;
		}
		constexpr until operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// Right fold: of op
	template <class BinOp, class I, class T = typename I::value_type>
	class fold {
		copy_assignable<BinOp> op;
		I i;
		T t;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr fold() = default;
		constexpr fold(BinOp op, const I& i, T t = 0)
			: op(std::move(op)), i(i), t(t)
		{ }
		constexpr fold(const fold& f) = default;
		constexpr fold& operator=(const fold& f) = default;
		constexpr fold(fold&& f) = default;
		constexpr fold& operator=(fold&& f) noexcept = default;
		constexpr ~fold() = default;

		constexpr bool operator==(const fold& f) const
		{
			return i == f.i && t == f.t;
		}

		constexpr explicit operator bool() const
		{
			return i.operator bool();
		}
		constexpr value_type operator*() const noexcept
		{
			return t;
		}
		constexpr fold& operator++() noexcept
		{
			if (i) {
				t = op(t, *i);
				++i;
			}

			return *this;
		}
		constexpr fold operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	template <class I, class T = typename I::value_type>
	inline auto sum(I i, T t = 0)
	{
		while (i) {
			t += *i;
			++i;
		}

		return t;
	}

	template <class I, class T = typename I::value_type>
	inline auto prod(I i, T t = 1)
	{
		while (i) {
			t *= *i;
			++i;
		}

		return t;
	}

	// d(i[1], i[0]), d(i[2], i[1]), ...
	template <class I, class T = typename I::value_type, class D = std::minus<T>, 
		typename U = std::invoke_result_t<D, T, T>>
	class delta {
		copy_assignable<D> d;
		I i;
		T t;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = U;
		using reference = U&;
		using pointer = U*;
		using difference_type = std::ptrdiff_t;

		constexpr delta() = default;
		constexpr delta(const I& _i, D _d = std::minus<T>{})
			: d(std::move(_d)), i(_i), t{}
		{
			if (i) {
				t = *i;
				++i;
			}
		}
		constexpr delta(const delta& _d) = default;
		constexpr delta& operator=(const delta& _d) = default;
		constexpr delta(delta&& _d) = default;
		constexpr delta& operator=(delta&& _d) = default;
		constexpr ~delta() = default;

		constexpr bool operator==(const delta& _d) const
		{
			return i == _d.i && t == _d.t;
		}

		constexpr explicit operator bool() const
		{
			return i.operator bool();
		}
		constexpr value_type operator*() const
		{
			return d(*i, t);
		}
		constexpr delta& operator++() noexcept
		{
			if (i) {
				t = *i;
				++i;
			}

			return *this;
		}
		constexpr delta operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

	// uptick + downtick = delta
	template <class I, class T = typename I::value_type>
	inline auto uptick(I i)
	{
		return delta(i, [](T a, T b) { return std::max<T>(b - a, 0); });
	}
	template <class I, class T = typename I::value_type>
	inline auto downtick(I i)
	{
		return delta(i, [](T a, T b) { return std::min<T>(b - a, 0); });
	}

	template<class... Is>
	class tuple {
		std::tuple<Is...> is;
	public:
		using iterator_category = std::input_iterator_tag;
		using value_type = std::tuple<typename Is::value_type...>;
		using reference = value_type&;
		using difference_type = std::ptrdiff_t;

		constexpr tuple(Is... is)
			: is(is...)
		{ }
		constexpr tuple(const tuple&) = default;
		constexpr tuple& operator=(const tuple&) = default;
		constexpr tuple(tuple&&) = default;
		constexpr tuple& operator=(tuple&&) = default;
		constexpr ~tuple() = default;

		constexpr bool operator==(const tuple& t) const = default;

		constexpr explicit operator bool() const
		{
			return std::apply([](auto... i) { return (i && ...); }, is);
		}
		constexpr value_type operator*() const
		{
			return std::apply([](auto... i) { return std::make_tuple(*i...); }, is);
		}
		constexpr tuple& operator++() noexcept
		{
			std::apply([](auto&... i) { (++i, ...); }, is);

			return *this;
		}
		constexpr tuple operator++(int) noexcept
		{
			auto tmp{ *this };

			operator++();

			return tmp;
		}
	};

} // namespace fms::iterable

#define FMS_ITERABLE_OPERATOR(X) \
    X(+, plus)         \
    X(-, minus)        \
    X(*, multiplies)   \
    X(/, divides)      \
    X(%, modulus)

#define FMS_ITERABLE_OPERATOR_FUNCTION(OP, OP_) \
    template <class I, class J, class T = std::common_type_t<std::iter_value_t<I>, std::iter_value_t<J>>> \
	constexpr auto operator OP(const I& i, const J& j) {  return fms::iterable::binop(std::OP_<T>{}, i, j); } \

FMS_ITERABLE_OPERATOR(FMS_ITERABLE_OPERATOR_FUNCTION)
#undef FMS_ITERABLE_OPERATOR_FUNCTION
#undef FMS_ITERABLE_OPERATOR

//template <class I, class T = std::iter_value_t<I>> 
//constexpr auto operator+(const I& i, T t)
//{
//	return fms::iterable::binop(std::plus<T>{}, i, fms::iterable::constant(t));
//}

template<class I, class T = std::iter_value_t<I>>
constexpr auto operator-(const I& i)
{
	return constant(T(-1)) * i;
}
