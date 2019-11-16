/** This module contains the $(LREF Dual) type, which is used to represent
    dual numbers, along with related mathematical operations and functions.

    The intention of the this module is to provide a dual number type, which
    can be used for automatic differentation. This means you can calculate
    with it the same way you would with floating point numbers. The real
    part will be identically to the result of a calculation with floating
    point numbers. Comparison operations (opCmp) ignore the value of the dual part.

    To calculate the derivate with respect to a variable, set the dual part
    of this variable to 1. All other dual parts should be 0. After the
    calculation the dual part contains the derivate.

    See_Also:
        https://en.wikipedia.org/wiki/Dual_number
        https://en.wikipedia.org/wiki/Automatic_differentiation#Automatic_differentiation_using_dual_numbers

    Current_limitations:
        "^^" operator works only for integral numbers.
        No trigonomertic functions.

    Authors:    René Heldmaier and others (see individual files)
    Copyright:  Copyright (c) 2019, René Heldmaier and others (see individual files)
    License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     https://github.com/the5avage/dualnumbers
*/

/* The functions

   auto dual(R)(const R re),
   auto dual(R, D)(const R re, const D du),
   string toString() const,
   void toString(Writer, Char)(scope Writer w, scope const ref FormatSpec!Char formatSpec) const

   and the corresponding unittests are derived from phobos std.complex
   (github.com/dlang/phobos/blob/master/std/complex.d).
   See comments for more details
*/

module dualnumbers;

import std.traits;
import std.math;

/* The following two functions and the corresponding unittest are derived from
   phobos std.complex (github.com/dlang/phobos/blob/master/std/complex.d)
   Authors:     Lars Tandle Kyllingstad, Don Clugston
   Copyright:   Copyright (c) 2010, Lars T. Kyllingstad.
   License:     $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
   Modified by: René Heldmaier
*/
/** Helper function that returns a dual number with the specified
    real and dual parts.
    Params:
        R = (template parameter) type of real part of dual number
        D = (template parameter) type of dual part of dual number
        re = real part of complex number to be constructed
        du = (optional) dual part of complex number, 0 if omitted.
    Returns:
        `Dual` instance with real and dual parts set
        to the values provided as input.  If neither `re` nor
        `du` are floating-point numbers, the return type will
        be `Dual!double`.  Otherwise, the return type is
        deduced using $(D std.traits.CommonType!(R, D)).
*/
auto dual(R)(const R re)  @safe pure nothrow @nogc
if (is(R : double))
{
    static if (isFloatingPoint!R)
        return Dual!R(re, 0);
    else
        return Dual!double(re, 0);
}

/// ditto
auto dual(R, D)(const R re, const D du)  @safe pure nothrow @nogc
if (is(R : double) && is(D : double))
{
    static if (isFloatingPoint!R || isFloatingPoint!D)
        return Dual!(CommonType!(R, D))(re, du);
    else
        return Dual!double(re, du);
}

///
@safe pure nothrow unittest
{
    auto a = dual(1.0);
    static assert(is(typeof(a) == Dual!double));
    assert(a.re == 1.0);
    assert(a.du == 0.0);

    auto b = dual(2.0L);
    static assert(is(typeof(b) == Dual!real));
    assert(b.re == 2.0L);
    assert(b.du == 0.0L);

    auto c = dual(1.0, 2.0);
    static assert(is(typeof(c) == Dual!double));
    assert(c.re == 1.0);
    assert(c.du == 2.0);

    auto d = dual(3.0, 4.0L);
    static assert(is(typeof(d) == Dual!real));
    assert(d.re == 3.0);
    assert(d.du == 4.0L);

    auto e = dual(1);
    static assert(is(typeof(e) == Dual!double));
    assert(e.re == 1);
    assert(e.du == 0);

    auto f = dual(1L, 2);
    static assert(is(typeof(f) == Dual!double));
    assert(f.re == 1L);
    assert(f.du == 2);

    auto g = dual(3, 4.0L);
    static assert(is(typeof(g) == Dual!real));
    assert(g.re == 3);
    assert(g.du == 4.0L);
}

/// Dual number for automatic differentation
struct Dual(T) if (isFloatingPoint!T)
{
    import std.format : FormatSpec;
    import std.range.primitives : isOutputRange;

    /// The real part
    T re;
    /// The dual part
    T du;

    /* The following two functions and the corresponding unittest are derived from
       phobos std.complex (github.com/dlang/phobos/blob/master/std/complex.d)
       Authors:     Lars Tandle Kyllingstad, Don Clugston
       Copyright:   Copyright (c) 2010, Lars T. Kyllingstad.
       License:     $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
       Modified by: René Heldmaier
    */
    /** Converts the dual number to a string representation.
        The second form of this function is usually not called directly;
        instead, it is used via $(REF format, std,string), as shown in the examples
        below.  Supported format characters are 'e', 'f', 'g', 'a', and 's'.
        See the $(MREF std, format) and $(REF format, std,string)
        documentation for more information.
    */
    string toString() const @safe /* TODO: pure nothrow */
    {
            import std.exception : assumeUnique;
            char[] buf;
            buf.reserve(100);
            auto fmt = FormatSpec!char("%s");
            toString((const(char)[] s) { buf ~= s; }, fmt);
            static trustedAssumeUnique(T)(T t) @trusted { return assumeUnique(t); }
            return trustedAssumeUnique(buf);
    }

    ///
    @safe unittest
    {
            auto c = dual(1.2, 3.4);

            // Vanilla toString formatting:
            assert(c.toString() == "1.2+3.4ε");

            // Formatting with std.string.format specs: the precision and width
            // specifiers apply to both the real and imaginary parts of the
            // complex number.
            import std.format : format;
            assert(format("%.2f", c)  == "1.20+3.40ε");
            assert(format("%4.1f", c) == " 1.2+ 3.4ε");
    }

    /// ditto
    void toString(Writer, Char)(scope Writer w, scope const ref FormatSpec!Char formatSpec) const
        if (isOutputRange!(Writer, const(Char)[]))
        {
            import std.format : formatValue;
            import std.math : signbit;
            import std.range.primitives : put;
            formatValue(w, re, formatSpec);
            if (signbit(du) == 0)
                put(w, "+");
            formatValue(w, du, formatSpec);
            put(w, "ε");
    }

    @safe pure nothrow @nogc:

    /**
    Construct a dual number with the specified real and
    dual parts. In the case where a single argument is passed
    that is not a dual number, the imaginary part of the result will be
    zero.
    */
    this(R : T)(const Dual!R d)
    {
        re = d.re;
        du = d.du;
    }

    /// ditto
    this(R : T)(const R re)
    {
        this.re = re;
        du = 0.0;
    }

    /// ditto
    this(Rr : T, Rd : T)(const Rr re, const Rd du)
    {
        this.re = re;
        this.du = du;
    }

    /* assignment operators */

    // this = numeric
    ref Dual opAssign(R : T)(const R rhs)
    {
        this.re = rhs;
        this.du = 0.0;
        return this;
    }

    // this = dual
    ref Dual opAssign(R : T)(const Dual!R rhs)
    {
        re = rhs.re;
        du = rhs.du;
        return this;
    }

    /* unary operators */

    // unary + and -
    Dual opUnary(string op)() const
        if (op == "+" || op == "-")
    {
        return mixin("Dual(" ~op~ "re," ~op~ "du)");
    }

    /* binary operators */

    // dual + dual, dual - dual
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const Dual!R rhs) const
        if (op == "+" || op == "-")
    {
        alias D = typeof(return);
        return mixin("D(re" ~ op ~ "rhs.re, du" ~ op ~ "rhs.du)");
    }

    // dual * dual
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const Dual!R rhs) const
        if (op == "*")
    {
        alias D = typeof(return);
        return D(re * rhs.re, du * rhs.re + re * rhs.du);
    }

    // dual / dual
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const Dual!R rhs) const
        if (op == "/")
    {
        alias D = typeof(return);
        return D(re/rhs.re, (rhs.re*du - rhs.du*re)/rhs.re/rhs.re);
    }

    // dual + numeric, dual - numeric
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const R rhs) const
        if ((op == "+" || op == "-") && isNumeric!R)
    {
        alias D = typeof(return);
        return mixin("D(re" ~op~ "rhs, du)");
    }

    // dual * numeric, dual / numeric
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const R rhs) const
        if ((op == "*" || op == "/") && isNumeric!R)
    {
        alias D = typeof(return);
        return mixin("D(re" ~op~ "rhs, du" ~op~ "rhs)");
    }

    // numeric * dual, numeric + dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
        if ((op == "+" || op == "*") && isNumeric!R)
    {
        return opBinary!op(lhs);
    }

    // numeric - dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
        if ((op == "-") && isNumeric!R)
    {
        alias D = typeof(return);
        return D(lhs - re, -du);
    }

    // numeric / dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
        if ((op == "/") && isNumeric!R)
    {
        alias D = typeof(return);
        return D(lhs/re, -du*lhs/re/re);
    }

    // dual ^^ integer
    Dual!T opBinary(string op, R)(R rhs) const
        if (op == "^^" && isIntegral!R)
    {
        if (rhs > 0) {
            Dual!T result = this;
            while (--rhs) {
                result *= this;
            }
            return result;
        } else if (rhs < 0) {
            auto result = Dual!T(1.0);
            while (rhs++) {
                result /= this;
            }
            return result;
        }
        return Dual!T(1.0);
    }

    /* op-assign operators */

    // dual += dual, dual -= dual, dual *= dual, dual /= dual
    ref Dual opOpAssign(string op, D)(const D rhs)
        if ((op == "+" || op == "-" || op == "*" || op == "/") && is (D R == Dual!R))
    {
        mixin("this = this " ~op~ "rhs;");
        return this;
    }

    // dual ^^= numeric
    ref Dual opOpAssign(string op, R : T)(R rhs)
        if (op == "^^")
    {
            this = this ^^ rhs;
            return this;
    }

    // dual += numeric, dual -= numeric, dual *= numeric, dual /= numeric
    ref Dual opOpAssign(string op, R : T)(const R rhs)
        if (op == "+" || op == "-" || op == "*" || op =="/")
    {
        mixin("this = this " ~op~ "rhs;");
        return this;
    }

    /* comparison operators
    Ignore dual part because dual numbers shall behave identically
    to normal numbers (they are made for automatic differentation).
    */

    // this == dual
    bool opEquals(R : T)(const Dual!R z) const
    {
        return re == z.re && du == z.du;
    }

    // this == numeric
    bool opEquals(R : T)(const R r) const
    {
        return re == r && du == 0;
    }

    // this == dual
    int opCmp(R : T)(const Dual!R z) const
    {
        if (re < z.re)
            return -1;
        else if (re > z.re)
            return 1;
        else
            return 0;
    }

    // this == dual
    int opCmp(R : T)(const R r) const
    {
            if (re < r)
                return -1;
            else if (re > r)
                return 1;
            else
                return 0;
    }
}

/** To calculate the derivate of a function, set the dual part of the variable
    by which you want to derive to 1. Other dual numbers should have dual part of 0.
    Then just calculate the function like you would with normal numbers.
    The real part is always identically to the result with normal numbers.
    After the calculation the dual part holds the derivate of the function.
*/
unittest
{
    import std.math: approxEqual;
    // f(x) = x⁵ f'(x) = 5x⁴ for x = 2
    const x = Dual!double(2.0, 1.0);
    auto f2 = x^^5;
    assert(f2.re.approxEqual(2.0^^5) && f2.du.approxEqual(5.0*2.0^^4));

    // f(x) = 3x² f'(x) = 6x for x = 2
    f2 = 3.0 * x * x;
    assert(approxEqual(f2.re, 3.0*2.0*2.0) && approxEqual(f2.du, 6.0*2.0));

    // f(x) = 3/(1-x) f'(x) = 3/(1-x)^2 for x = 2
    f2 = 3.0/(1.0 - x);
    assert(f2.re.approxEqual(-3.0) && f2.du.approxEqual(3.0));

    // f(x) = 3exp(2x), f'(x) = 6exp(2x) for x = 2
    f2 = 3 * exp(2 * x);
    import std.math: exp;
    assert(f2.re.approxEqual(3 * exp(4.0)) && f2.du.approxEqual(6 * exp(4.0)));
}

/** Exponential function on dual numbers.

    Params: x = A dual number.
    Returns: exp(x)
*/
Dual!T exp(T)(Dual!T x)  @safe pure nothrow @nogc
{
    import std.math: exp;
    Dual!T result = void;
    result.re = exp(x.re);
    result.du = result.re * x.du;
    return result;
}

///
unittest
{
    import std.math: approxEqual;
    // f(x) = exp(x), f'(x) = exp(x)
    auto x = dual(5.0, 1.0);
    auto res = x.exp();
    assert(res.re.approxEqual(res.du));

    // f(x) = exp(3x), f'(x) = 3*exp(3x)
    res = exp(3 * x);
    assert(res.du.approxEqual(3 * res.re));
}

/** Abs function on dual numbers.

    Params: x = A dual number.
    Returns: |x|
*/
Dual!T abs(T)(Dual!T x)  @safe pure nothrow @nogc
{
    import std.math: signbit;
    if (signbit(x.re))
        return -x;
    else
        return x;
}

///
unittest
{
    import std.math: approxEqual;
    // f(x) = |x|, f'(x) = 1 when x positive
    auto x = dual(2.0, 1.0);
    auto result = abs(x);
    assert(approxEqual(result.re, 2.0) && approxEqual(result.du, 1.0));

    // f'(x) = -1 when x negative
    x = dual(-2.0, 1.0);
    result = abs(x);
    assert(approxEqual(result.re, 2.0) && approxEqual(result.du, -1.0));

    // because floating point numbers have -0 and +0 f'(x) is defined for x = 0
    x = dual(0.0, 1.0);
    result = abs(x);
    assert(approxEqual(result.du, 1.0));
    x = dual(-0.0, 1.0);
    result = abs(x);
    assert(approxEqual(result.du, -1.0));
}

/* Trigonometric functions */

/** Sinus function on dual numbers.

    Params: x = A dual number.
    Returns: sin(x)
*/
Dual!T sin(T)(Dual!T x)  @safe pure nothrow @nogc
{
    import std.math: sin, cos;
    Dual!T result = void;
    result.re = sin(x.re);
    result.du = x.du * cos(x.re);
    return result;
}

///
unittest
{
    import std.math;
    // f(x) = sin(x), f'(x) = cos(x)
    auto x = dual(0.0, 1.0);
    auto result = sin(x);
    assert(approxEqual(result.re, 0.0)); // sin(0) = 0
    assert(approxEqual(result.du, 1.0)); // cos(0) = 1

    x = dual(PI / 2.0, 1.0);
    result = sin(x);
    assert(approxEqual(result.re, 1.0)); // sin(π/2) = 1
    assert(approxEqual(result.du, 0.0)); // cos(π/2) = 0

    // f(x) = sin(3x), f'(x) = 3cos(3x)
    x = dual(5.0, 1.0);
    result = sin(3 * x);
    assert(approxEqual(result.re, std.math.sin(15.0)));
    assert(approxEqual(result.du, 3.0 * std.math.cos(15.0)));
}

/* Test constructors */

// Works with floating point types
unittest
{
    Dual!float df;
    Dual!double dd;
    Dual!real dr;
    assert(df.sizeof == 2*float.sizeof);
    assert(dd.sizeof == 2*double.sizeof);
    assert(dr.sizeof == 2*real.sizeof);
}

// Constructor with one parameter
unittest
{
    import std.math: approxEqual;
    Dual!double d = Dual!double(33.3);
    assert(d.re == 33.3 && d.du == 0.0);
    Dual!real dr = Dual!real(44.4);
    assert(dr.re.approxEqual(44.4) && dr.du.approxEqual(0.0));
}

// Constructor with two parameters
unittest
{
    import std.math: approxEqual;
    Dual!double d = Dual!double(1.1, 2.2);
    assert(d.re.approxEqual(1.1) && d.du.approxEqual(2.2));
}

// Construct from other dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.0, 2.0);
    Dual!float f = d;
    assert(d.re.approxEqual(f.re) && d.du.approxEqual(f.du));
}

/* Test operators */

// dual = numeric
unittest
{
    import std.math: approxEqual;
    immutable f = 4.0;
    auto d = Dual!double(0.9, 0.9);
    auto d2 = d = f;
    assert(d2.re.approxEqual(4.0) && d2.du.approxEqual(0.0));
}

// dual = dual
unittest
{
    auto d = Dual!double(1.1, 1.1);
    const  d2 = Dual!double(2.2, 3.3);
    d = d2;
    assert(d.re == 2.2 && d.du == 3.3);
}

// Chain assignment
unittest
{
    import std.math: approxEqual;
    Dual!double dd = Dual!double(1.1, 2.2);
    Dual!float df;
    df = dd = Dual!real(1.1, 2.2);
    assert(df.re.approxEqual(1.1) && df.du.approxEqual(2.2));
}

// unary + and -
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    Dual!double dp = +d;
    assert(approxEqual(dp.re, 1.1) && approxEqual(dp.du, 2.2));
    Dual!double dm = -d;
    assert(approxEqual(dm.re, -1.1) && approxEqual(dm.du, -2.2));
}

// dual + dual
unittest
{
    import std.math: approxEqual;
    auto d = Dual!double(1.1, 2.2);
    const d2 = Dual!float(3.3, 4.4);
    d = d2 + d;
    assert(approxEqual(d.re, 4.4) && approxEqual(d.du, 6.6));
}

// dual - dual
unittest
{
    import std.math: approxEqual;
    auto d = Dual!double(1.1, 2.2);
    const d2 = Dual!float(3.3, 5.5);
    d = d2 - d;
    assert(approxEqual(d.re, 2.2) && approxEqual(d.du, 3.3));
}

// dual + numeric, numeric + dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    auto res = d + 5.0;
    assert(approxEqual(res.re, 6.1) && approxEqual(res.du, 2.2));
    res = 1.0 + d;
    assert(approxEqual(res.re, 2.1) && approxEqual(res.du, 2.2));
}

// dual - numeric, numeric - dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    auto res = d - 5.0;
    assert(approxEqual(res.re, -3.9) && approxEqual(res.du, 2.2));
    res = 1.0 - d;
    assert(approxEqual(res.re, -0.1) && approxEqual(res.du, -2.2));
}

// dual * dual
unittest
{
    import std.math: approxEqual;
    const a = Dual!double(1.1, 2.2);
    const b = Dual!float(3.3, 4.4);
    Dual!double d = a * b;
    assert(approxEqual(d.re, 1.1 * 3.3) && approxEqual(d.du, 1.1*4.4 + 2.2*3.3));
}

// dual / dual
unittest
{
    import std.math: approxEqual;
    const a = Dual!double(1.1, 2.2);
    const b = Dual!float(3.3, 4.4);
    Dual!double d = a / b;
    assert(approxEqual(d.re, 1.1 / 3.3) && approxEqual(d.du, (3.3*2.2 - 1.1*4.4)/3.3/3.3));
}

// dual * numeric
unittest
{
    import std.math: approxEqual;
    Dual!double d = Dual!double(1.1, 2.2);
    d = d * 3.0;
    assert(approxEqual(d.re, 3.3) && approxEqual(d.du, 6.6));
}

// numeric * dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    const res = 3.0 * d;
    assert(approxEqual(res.re, 3.3) && approxEqual(res.du, 6.6));
}

// dual / numeric
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    const res = d / 3.0;
    assert(approxEqual(res.re, 1.1/3.0) && approxEqual(res.du, 2.2/3.0));
}

// numeric / dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    Dual!double expect = Dual!double(3.0, 0.0);
    Dual!double res = 3.0 / d;
    expect = expect / d;
    assert(approxEqual(res.re, expect.re) && approxEqual(res.du, expect.du));
}

// dual += dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    Dual!double res = d;
    res += d;
    assert(res.re.approxEqual(2.2) && res.du.approxEqual(4.4));
}

// dual -= dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    auto res = Dual!double(0.5, 0.7);
    res -= d;
    assert(res.re.approxEqual(-0.6) && res.du.approxEqual(-1.5));
}

// dual *= dual
unittest
{
    import std.math: approxEqual;
    const d = Dual!double(1.1, 2.2);
    auto res = Dual!double(0.5, 0.7);
    auto expect = res * d;
    res *= d;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual /= dual
unittest
{
    import std.math: approxEqual;
    immutable d = Dual!double(1.1, 2.2);
    auto res = Dual!double(0.5, 0.7);
    auto expect = res / d;
    res /= d;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual += numeric
unittest
{
    import std.math: approxEqual;
    auto res = Dual!double(0.5, 0.7);
    res += 5.0;
    assert(res.re.approxEqual(5.5) && res.du.approxEqual(0.7));
}

// dual -= numeric
unittest
{
    import std.math: approxEqual;
    auto res = Dual!double(0.5, 0.7);
    res -= 5.0;
    assert(res.re.approxEqual(-4.5) && res.du.approxEqual(0.7));
}

// dual *= numeric
unittest
{
    import std.math: approxEqual;
    auto res = Dual!double(0.5, 0.7);
    auto expect = res * 5.0;
    res *= 5.0;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual /= numeric
unittest
{
    import std.math: approxEqual;
    auto res = Dual!double(0.5, 0.7);
    auto expect = res / 5.0;
    res /= 5.0;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual ^^= numeric
unittest
{
    import std.math: approxEqual;
    auto res = Dual!double(0.5, 0.7);
    auto expect = res ^^ 5;
    res ^^= 5;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual ^^ positive integer
unittest
{
    import std.math: approxEqual;
    long exponent = 4;
    const d = Dual!double(2.2, 3.3);
    const expect = d * d * d * d;
    const res = d ^^ exponent;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// dual ^^ 0
unittest
{
    import std.math: approxEqual;
    short exponent = 0;
    const d = Dual!double(3.3, 2.2);
    const res = d ^^ exponent;
    assert(res.re.approxEqual(1.0) && res.du.approxEqual(0.0));
}

// dual ^^ negative integer
unittest
{
    import std.math: approxEqual;
    int exponent = -4;
    const d = Dual!double(2.2, 3.3);
    const expect = 1.0 / d / d / d / d;
    const res = d ^^ exponent;
    assert(res.re.approxEqual(expect.re) && res.du.approxEqual(expect.du));
}

// opEquals
unittest
{
    const a = Dual!double(1.1, 2.2);
    const b = Dual!double(1.1, 3.3);
    const c = a;
    const d = Dual!float(1.1f, 0.0f);
    assert(a != b && a == c);
    assert(d == 1.1f && a != 1.1);
}

// opCmp
unittest
{
    const a = Dual!double(1.1, 100.0);
    const b = Dual!double(2.0, -30.0);
    assert(a < b && a <= b);
    assert(b > a && b >= a);
    assert(a <= a && a >= a);
    assert(a < 2.0 && a <= 2.0);
    assert(2.0 > a && 2.0 >= a);
    assert(a <= 1.1 && a >= 1.1);
}
