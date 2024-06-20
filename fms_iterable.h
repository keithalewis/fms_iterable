// fms_iterable.h - iterators with operator bool() sentinel
#pragma once
#include <compare>
#include <concepts>
#include <initializer_list>
#include <iterator>
#include <utility>

namespace fms::iterable {

	template<class I>
	concept input_iterable = std::input_iterator<I> &&
		requires(I i) {
		{ i.operator bool() } -> std::same_as<bool>;
	};
	template<class I, class T>
	concept output_iterable = std::output_iterator<I, T> &&
		requires(I i) {
			{ i.operator bool() } -> std::same_as<bool>;
	};
	template<class I>
	concept forward_iterable = std::forward_iterator<I> &&
		requires(I i) {
			{ i.operator bool() } -> std::same_as<bool>;
	};
	template<class I>
	concept bidirectional_iterable = std::bidirectional_iterator<I> &&
		requires(I i) {
			{ i.operator bool() } -> std::same_as<bool>;
	};
	template<class I>
	concept random_access_iterable = std::random_access_iterator<I> &&
		requires(I i) {
			{ i.operator bool() } -> std::same_as<bool>;
	};
	template<class I>
	concept contiguous_iterable = std::contiguous_iterator<I> &&
		requires(I i) {
			{ i.operator bool() } -> std::same_as<bool>;
	};

	// Compare elements of two iterables
	template<class I, class J>
	constexpr auto compare(I i, J j)
	{
		while (i && j) {
			const auto cmp = *i++ <=> *j++;
			if (cmp != 0) {
				return cmp;
			}
		}

		return !!i <=> !!j;
	}


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
		constexpr ~ptr() = default;

		constexpr auto operator<=>(const ptr& _p) const = default;

		constexpr ptr begin() const
		{
			return *this;
		}
		constexpr ptr end() const
		{
			return ptr(nullptr);
		}

		constexpr explicit operator bool() const noexcept
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
		reference operator[](difference_type i) const noexcept
		{
			return p[i];
		}
		reference operator[](difference_type i) noexcept
		{
			return p[i];
		}
	};
	template<class T>
	constexpr ptr<T> operator+(std::ptrdiff_t d, ptr<T> p) noexcept
	{
		return p + d;
	}
	template<class T>
	constexpr ptr<T> operator-(std::ptrdiff_t d, ptr<T> p) noexcept
	{
		return p - d;
	}

	// Iterable with no elements.
	template<class T>
	constexpr auto empty()
	{
		return ptr<T>();
	}

	// Iterable over [begin(), end()).
	template<class I>
	class interval : public I {
		I e;
	public:
		constexpr interval() = default;
		constexpr interval(I b, I e)
			: I(b), e(e)
		{ }
		constexpr interval(const interval&) = default;
		constexpr interval& operator=(const interval&) = default;
		constexpr interval(interval&&) = default;
		constexpr interval& operator=(interval&&) = default;
		constexpr ~interval() = default;

		/*constexpr*/ auto operator<=>(const interval& i) const = default;

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
		return interval(std::begin(c), std::end(c));
	}

	// Assumes lifetime of a.
	template<class T, std::size_t N>
	constexpr auto array(T(&a)[N]) noexcept
	{
		return interval(ptr(a), ptr(a + N));
	}

	template<class I>
	constexpr auto take(I i, std::size_t n)
	{
		return interval(i, std::next(i, n));
	}

	// i0, ..., in, i0, ...
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
		// no end

		constexpr explicit operator bool() const noexcept
		{
			return i.operator bool(); // repeat(empty) = empty
		}
		constexpr value_type operator*() const noexcept
		{
			return *i;
		}
		constexpr reference operator*() noexcept
			requires std::indirectly_writable<I,value_type>
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

	template<class T>
	class constant {
		T t;
	public:
		using iterator_category = std::forward_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;

		constexpr constant() = default;	
		constexpr constant(T t)
			: t(t)
		{ }	

		constexpr auto operator<=>(const constant&) const = default;

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
	};

	template<class T>
	constexpr auto once(T t)
	{
		return take(constant<T>(t), 1);
	}


} // namespace fms
