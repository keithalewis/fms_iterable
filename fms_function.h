// fms_function.h - functions for iterables
#pragma once
#include "fms_iterable.h"

namespace fms {

#pragma region all

	// return first false element or end
	template<iterable I>
	constexpr I all(I i)
	{
		return !i or !*i ? i : all(++i);
	}

#ifdef _DEBUG

	inline int all_test()
	{
		constexpr int i[] = { 1, 2, 3 };
		{
			constexpr auto is = array(i);
			static_assert(*is == i[0]);
			//constexpr auto aa = all(is);
			//static_assert(all(is) == is.end());
			//assert(!all(i.end()));
		}

	}

#endif // _DEBUG

#pragma endregion all

#pragma region any

	// return end or first true element
	template<iterable I>
	inline constexpr I any(I i)
	{
		return !i or *i ? i : any(++i);
	}

#ifdef _DEBUG

	template<iterable I>
	inline bool test_any(const I& is)
	{
		bool b = false; // any({}) = false

		for (auto i : is) {
			if (i) {
				b = true;
				break;
			}
		}

		auto as = any(is);

		if (!b) {
			assert(!as);
			assert(as == is.end());
		}
		else {
			assert(as);
			assert(*as);
		}
	}

#endif // _DEBUG

#pragma endregion any

#pragma region last

	// last before end
	template<iterable I>
	inline I last(I i)
	{
		for (I i_ = i; ++i_; i = i_) ; // nothing

		return i;
	}

#ifdef _DEBUG

	template<iterable I>
	inline bool test_last(I i)
	{
		{
			assert(++last(i) == i.end());
		}

		return true;
	}

#endif // _DEBUG

#pragma region last

#pragma region skip

	namespace {
		template<iterable I>
		inline I skip_tag(typename I::difference_type n, I i, std::forward_iterator_tag)
		{
			// ensure(n >= 0);
			while (n and i) {
				--n;
				++i;
			}

			return i;
		}
		template<iterable I>
		inline I skip_tag(typename I::difference_type n, I i, std::random_access_iterator_tag)
		{
			return i += n;
		}
	}
	template<iterable I>
	inline I skip(typename I::difference_type n, I i)
	{
		return skip_tag(n, i, typename I::iterator_category{});
	}

#pragma endregion skip

#pragma region size_length

	// number of items in sequence
	// size(s, size(t)) = size(s) + size(t)
	template<iterable I>
	inline size_t size(I i, size_t n = 0)
	{
		for (; i; ++i) { ++n; }

		return n;
	}
	// template<sized_iterable I> inline size_t size(const I& i) { return i.size(); }

	// size alias
	template<iterable I>
	inline size_t length(I i, size_t n = 0)
	{
		return size(i, n);
	}

#ifdef _DEBUG

	template<iterable I, iterable J>
	inline bool test_size(const I& i, const J& j)
	{
		assert(size(i, size(j)) == size(i) + size(j));

		return true;
	}

#endif // _DEBUG

#pragma endregion size_length

#pragma region copy

	// copy i to STL output iterator
	template<iterable I, typename J>
	//requires std::output_iterator<J, typename J::value_type>
	inline bool copy(I i, J j)
	{
		for (; i; ++i, ++j) {
			*j = *i;
		}

		return true;
	}
	// copy i to j and indicate complete copy
	template<iterable I, iterable J>
		//requires std::output_iterator<J, typename J::value_type>
	inline bool copy(I i, J j)
	{
		for (; i and j; ++i, ++j) {
			*j = *i;
		}

		return !i and !j;
	}
	
#ifdef _DEBUG

	bool test_copy()
	{
		return true;
	}

#endif // _DEBUG

#pragma region copy

}
