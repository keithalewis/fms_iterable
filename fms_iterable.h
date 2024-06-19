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
	template<input_iterable I, input_iterable J>
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
	template<input_iterable I, input_iterable J>
	constexpr auto operator<=>(I i, J j)
	{
		return !!i <=> !!j;
	}


	// Possibly unsafe pointer iterable with std::span/view semantics.
	template<class T>
	class ptr {
	protected:
		T* p;
		std::size_t n;
	public:
		using iterator_category = std::contiguous_iterator_tag;
		using value_type = T;
		using reference = T&;
		using pointer = T*;
		using difference_type = std::ptrdiff_t;
		
		constexpr ptr(T* p = nullptr, std::size_t n = 0)
			: p(p), n(n)
		{ }
		constexpr ptr(const ptr&) = default;
		constexpr ptr& operator=(const ptr&) = default;
		constexpr ptr(ptr&&) = default;
		constexpr ptr& operator=(ptr&&) = default;
		constexpr ~ptr() = default;

		//constexpr auto operator<=>(const ptr& _p) const
		//{
		//	return compare(*this, _p);
		//}
		constexpr bool operator==(const ptr& _p) const
		{
			return compare(*this, _p) == 0;
		}
		constexpr bool operator<(const ptr& _p) const
		{
			return compare(*this, _p) < 0;
		}

		constexpr ptr begin() const
		{
			return *this;
		}
		constexpr ptr end() const
		{
			return ptr(p + n, 0);
		}

		constexpr explicit operator bool() const noexcept
		{
			return n > 0;
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
			--n;

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
			++n;

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
			n -= d;

			return *this;
		}
		constexpr ptr operator+(difference_type d) const noexcept
		{
			return ptr(p + d, n - d);
		}
		constexpr ptr& operator-=(difference_type d) noexcept
		{
			p -= d;
			n += d;

			return *this;
		}
		constexpr ptr operator-(difference_type d) const noexcept
		{
			return ptr(p - d, n + d);
		}
		constexpr difference_type operator-(const ptr& i) const noexcept
		{
			return p - i.p;
		}
		constexpr reference operator[](difference_type i) const noexcept
		{
			return p[i];
		}
		constexpr reference operator[](difference_type i) noexcept
		{
			return p[i];
		}
		// contiguous
		constexpr auto operator->() const noexcept
		{
			return p;
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

	// Assumes lifetime of a.
	template<class T, std::size_t N>
	constexpr auto array(T(&a)[N]) noexcept
	{
		return ptr(a, N);
	}


} // namespace fms
