# iterable

> _All iterables begin alike, but each iterable ends after its own fashion_

This header only library uses C++20 to make iterators easier to use by
adding the member function `explicit operator bool() const` instead of using the STL `end()`
notion to delimit ranges.

Iterables allow for infinite (lazy) ranges. This is not a new idea.
Many other languages use something similar, in particular C# where it is used
in [`LINQ`](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/).
Perhaps the idea originated in lisp where it takes the form of `cdr` being empty.

Using iterables rather than arrays makes C++ more expressive. For example, 
`sum(epsilon(power(x)/factorial(0.)))` computes `exp(x)` to machine precision 
using exp(_x_) = &Sigma;<sub>_n_ ≥ 0</sub> _x_<sup>_n_</sup>/_n_!. The iterable
`power(x)` is the infinite sequence `x^n` for `n = 0, 1, 2, ...` and `factorial(0.)`
is the infinite sequence `n!`. Global arithmetic operators
are provided for element-wise operations. The quotient results in the infinite
sequence `x^n/n!`. The iterable transformation `epsilon` terminates an iterable
when a value less than machine epsilon is encountered. The function `sum` adds
all iterable values.

A `C` array can be made into an iterable by supplying its size. The library provides for constant
iterables and an infinite arithmetic sequences called `iota(t = 0)` to generate 
`t, t + 1, t + 2, ...`.
Overloads for arithmetic operators are provided so, for example, 
`a + b * iota(0)` results in `a + b*n`, `n = 0, 1, 2, ...` during traversal.
This permits certain peephole optimizations such as `operator+(0, i)`
returning `i`, or `operator*(i, 0)` returning the constant `0` iterator.

Writing an iterator class is simple: provide `explicit operator bool() const`,
`operator*() const` to return the current value, and `operator++()` to
increment to the next value in an appropriate way and return `*this`.
To allow using an iterable with the STL an `end() const` function should
also be provided.

The global `begin(iterable i)` simply returns `i` and the global `end(iterable i)`
returns `i.end()`.
This makes iterables usable with range based for loops. For example,

```
auto generate(iterable it) 
{
	for (const auto& i : it) {
		co_yield i;
	}
}
```
is an asynchronous generator coroutine.

As you will see when you peruse the code, most functions involving iterables have a natural and pleasing implementation.
