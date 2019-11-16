# dualnumbers
>A dual number type for automatic differentiation.

[![Coverage Status](https://coveralls.io/repos/github/the5avage/dualnumbers/badge.svg?branch=master)](https://coveralls.io/github/the5avage/dualnumbers?branch=master)

## Introduction
Dual numbers are similar to complex numbers a pair of two numbers. A dual number is written as a + &epsi;b, where a is 
called the real part and b is called the dual part of the number. &epsi;&sup2; is defined to be zero.

Given a function f(x) and a value f(a) which is the result of the function for x = a,
then f(a + &epsi;b) is equal to f(a) + &epsi;b * f **'** (a). This can be shown with a Taylor series.

In practice this means that we get the derivative of an arbitrary complicated function for free, if we calculate it with a dual number.
All we have to do is to set the dual part of the variable, by which we want to differentiate,
to one (all other dual parts should be zero). The result of the function will contain its derivative as the dual part.

For more information about theory see   
https://en.wikipedia.org/wiki/Dual_number  
https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers

## Documentation

The documentation can be found [here](https://dualnumbers.dpldocs.info/dualnumbers.html).
There are also some [examples](https://dualnumbers.dpldocs.info/dualnumbers.Dual.html#examples) in the documentation.

## Current Limitations

* ^^ operator only works for integral exponents
* No trigonometric functions

## Contributing

Feel free to create issues or pull requests. A unittest is expected for every added function.

## Licence

Distributed under the Boost Software Licence. See [licence](LICENCE.txt) file for more information.
