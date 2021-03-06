
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		var name = v.func ? v.func.name : v.name;
		return '<function' + (name === '' ? '' : ':') + name + '>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;
	
	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}	
	
	return _elm_lang$core$Native_List.fromArray(is);
}

function toInt(s)
{
	var len = s.length;
	if (len === 0)
	{
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
	}
	var start = 0;
	if (s[0] === '-')
	{
		if (len === 1)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
		}
		start = 1;
	}
	for (var i = start; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int" );
		}
	}
	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function toFloat(s)
{
	var len = s.length;
	if (len === 0)
	{
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
	}
	var start = 0;
	if (s[0] === '-')
	{
		if (len === 1)
		{
			return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
		}
		start = 1;
	}
	var dotCount = 0;
	for (var i = start; i < len; ++i)
	{
		var c = s[i];
		if ('0' <= c && c <= '9')
		{
			continue;
		}
		if (c === '.')
		{
			dotCount += 1;
			if (dotCount <= 1)
			{
				continue;
			}
		}
		return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float" );
	}
	return _elm_lang$core$Result$Ok(parseFloat(s));
}

function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		var value = result._0;
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		currentSend(incomingValue);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _NoRedInk$rocket_update$Rocket_ops = _NoRedInk$rocket_update$Rocket_ops || {};
_NoRedInk$rocket_update$Rocket_ops['=>'] = F2(
	function (v0, v1) {
		return {ctor: '_Tuple2', _0: v0, _1: v1};
	});
var _NoRedInk$rocket_update$Rocket$batchCommands = function (_p0) {
	var _p1 = _p0;
	return A2(
		_NoRedInk$rocket_update$Rocket_ops['=>'],
		_p1._0,
		_elm_lang$core$Platform_Cmd$batch(_p1._1));
};
var _NoRedInk$rocket_update$Rocket$batchInit = _NoRedInk$rocket_update$Rocket$batchCommands;
var _NoRedInk$rocket_update$Rocket$batchUpdate = function (fn) {
	return function (_p2) {
		return _NoRedInk$rocket_update$Rocket$batchCommands(
			fn(_p2));
	};
};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _Skinney$elm_array_exploration$Native_JsArray = function() {

var empty = [];

function singleton(val) {
    return [val];
}

function length(arr) {
    return arr.length;
}

function initialize(size, offset, f) {
    var result = newArray(size);

    for (var i = 0; i < size; i++) {
        result[i] = f(offset + i);
    }

    return result;
}

function newArray(size) {
    // A JS literal is much faster than `new Array(size)` in Safari.
    // The following code optimizes the common case of 32-sized arrays,
    // while falling back to the "proper" way to preallocate arrays
    // for other sizes. This makes a big performance difference in
    // Safari, while exerting a minor performance hit in Chrome.
    // For 32-sized arrays, Chrome and Safari become equally fast.
    if (size !== 32) {
        return new Array(size);
    }

    return [
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null, null, null, null,
        null, null
    ];
}

function initializeFromList(max, ls) {
    var result = newArray(max);

    for (var i = 0; i < max; i++) {
        if (ls.ctor === '[]') {
            result.length = i;
            break;
        }

        result[i] = ls._0;
        ls = ls._1;
    }

    return {
        ctor: '_Tuple2',
        _0: result,
        _1: ls
    };
}

function unsafeGet(idx, arr) {
    return arr[idx];
}

function unsafeSet(idx, val, arr) {
    var result = arr.slice();
    result[idx] = val;
    return result;
}

function push(val, arr) {
    var result = arr.slice();
    result.push(val);
    return result;
}

function foldl(f, init, arr) {
    var acc = init;
    var len = arr.length;

    for (var i = 0; i < len; i++) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function foldr(f, init, arr) {
    var acc = init;

    for (var i = arr.length - 1; i >= 0; i--) {
        acc = A2(f, arr[i], acc);
    }

    return acc;
}

function map(f, arr) {
    var len = arr.length;
    var result = newArray(len);

    for (var i = 0; i < len; i++) {
        result[i] = f(arr[i]);
    }

    return result;
}

function indexedMap(f, offset, arr) {
    var len = arr.length;
    var result = newArray(len);

    for (var i = 0; i < len; i++) {
        result[i] = A2(f, offset + i, arr[i]);
    }

    return result;
}

function slice(from, to, arr) {
    return arr.slice(from, to);
}

function appendN(n, dest, source) {
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length) {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = newArray(size);

    for (var i = 0; i < destLen; i++) {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++) {
        result[i + destLen] = source[i];
    }

    return result;
}

return {
    empty: empty,
    singleton: singleton,
    length: length,
    initialize: F3(initialize),
    initializeFromList: F2(initializeFromList),
    unsafeGet: F2(unsafeGet),
    unsafeSet: F3(unsafeSet),
    push: F2(push),
    foldl: F3(foldl),
    foldr: F3(foldr),
    map: F2(map),
    indexedMap: F3(indexedMap),
    slice: F3(slice),
    appendN: F3(appendN)
};

}();

var _Skinney$elm_array_exploration$Array_JsArray$appendN = _Skinney$elm_array_exploration$Native_JsArray.appendN;
var _Skinney$elm_array_exploration$Array_JsArray$slice = _Skinney$elm_array_exploration$Native_JsArray.slice;
var _Skinney$elm_array_exploration$Array_JsArray$indexedMap = _Skinney$elm_array_exploration$Native_JsArray.indexedMap;
var _Skinney$elm_array_exploration$Array_JsArray$map = _Skinney$elm_array_exploration$Native_JsArray.map;
var _Skinney$elm_array_exploration$Array_JsArray$foldr = _Skinney$elm_array_exploration$Native_JsArray.foldr;
var _Skinney$elm_array_exploration$Array_JsArray$foldl = _Skinney$elm_array_exploration$Native_JsArray.foldl;
var _Skinney$elm_array_exploration$Array_JsArray$push = _Skinney$elm_array_exploration$Native_JsArray.push;
var _Skinney$elm_array_exploration$Array_JsArray$unsafeSet = _Skinney$elm_array_exploration$Native_JsArray.unsafeSet;
var _Skinney$elm_array_exploration$Array_JsArray$unsafeGet = _Skinney$elm_array_exploration$Native_JsArray.unsafeGet;
var _Skinney$elm_array_exploration$Array_JsArray$initializeFromList = _Skinney$elm_array_exploration$Native_JsArray.initializeFromList;
var _Skinney$elm_array_exploration$Array_JsArray$initialize = _Skinney$elm_array_exploration$Native_JsArray.initialize;
var _Skinney$elm_array_exploration$Array_JsArray$length = _Skinney$elm_array_exploration$Native_JsArray.length;
var _Skinney$elm_array_exploration$Array_JsArray$singleton = _Skinney$elm_array_exploration$Native_JsArray.singleton;
var _Skinney$elm_array_exploration$Array_JsArray$empty = _Skinney$elm_array_exploration$Native_JsArray.empty;
var _Skinney$elm_array_exploration$Array_JsArray$JsArray = function (a) {
	return {ctor: 'JsArray', _0: a};
};

var _Skinney$elm_array_exploration$Array_Hamt$emptyBuilder = {
	tail: _Skinney$elm_array_exploration$Array_JsArray$empty,
	nodeList: {ctor: '[]'},
	nodeListSize: 0
};
var _Skinney$elm_array_exploration$Array_Hamt$translateIndex = F2(
	function (idx, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		var posIndex = (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? (_p2 + idx) : idx;
		return (_elm_lang$core$Native_Utils.cmp(posIndex, 0) < 0) ? 0 : ((_elm_lang$core$Native_Utils.cmp(posIndex, _p2) > 0) ? _p2 : posIndex);
	});
var _Skinney$elm_array_exploration$Array_Hamt$foldl = F3(
	function (f, init, _p3) {
		var _p4 = _p3;
		var helper = F2(
			function (i, acc) {
				var _p5 = i;
				if (_p5.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, acc, _p5._0);
				} else {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, f, acc, _p5._0);
				}
			});
		var acc = A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, init, _p4._2);
		return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, f, acc, _p4._3);
	});
var _Skinney$elm_array_exploration$Array_Hamt$foldr = F3(
	function (f, init, _p6) {
		var _p7 = _p6;
		var acc = A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, f, init, _p7._3);
		var helper = F2(
			function (i, acc) {
				var _p8 = i;
				if (_p8.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p8._0);
				} else {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, f, acc, _p8._0);
				}
			});
		return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p7._2);
	});
var _Skinney$elm_array_exploration$Array_Hamt$toIndexedList = function (_p9) {
	var _p10 = _p9;
	var helper = F2(
		function (n, _p11) {
			var _p12 = _p11;
			var _p13 = _p12._0;
			return {
				ctor: '_Tuple2',
				_0: _p13 - 1,
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _p13, _1: n},
					_1: _p12._1
				}
			};
		});
	return _elm_lang$core$Tuple$second(
		A3(
			_Skinney$elm_array_exploration$Array_Hamt$foldr,
			helper,
			{
				ctor: '_Tuple2',
				_0: _p10._0 - 1,
				_1: {ctor: '[]'}
			},
			_p10));
};
var _Skinney$elm_array_exploration$Array_Hamt$toList = function (arr) {
	return A3(
		_Skinney$elm_array_exploration$Array_Hamt$foldr,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		arr);
};
var _Skinney$elm_array_exploration$Array_Hamt$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var _Skinney$elm_array_exploration$Array_Hamt$length = function (_p14) {
	var _p15 = _p14;
	return _p15._0;
};
var _Skinney$elm_array_exploration$Array_Hamt$isEmpty = function (_p16) {
	var _p17 = _p16;
	return _elm_lang$core$Native_Utils.eq(_p17._0, 0);
};
var _Skinney$elm_array_exploration$Array_Hamt$branchFactor = 32;
var _Skinney$elm_array_exploration$Array_Hamt$shiftStep = _elm_lang$core$Basics$ceiling(
	A2(
		_elm_lang$core$Basics$logBase,
		2,
		_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor)));
var _Skinney$elm_array_exploration$Array_Hamt$bitMask = 4294967295 >>> (32 - _Skinney$elm_array_exploration$Array_Hamt$shiftStep);
var _Skinney$elm_array_exploration$Array_Hamt$getHelp = F3(
	function (shift, idx, tree) {
		getHelp:
		while (true) {
			var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
			var _p18 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			if (_p18.ctor === 'SubTree') {
				var _v10 = shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_v11 = idx,
					_v12 = _p18._0;
				shift = _v10;
				idx = _v11;
				tree = _v12;
				continue getHelp;
			} else {
				return A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, _p18._0);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$get = F2(
	function (idx, _p19) {
		var _p20 = _p19;
		var _p21 = _p20._0;
		return ((_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(idx, _p21) > -1)) ? _elm_lang$core$Maybe$Nothing : ((_elm_lang$core$Native_Utils.cmp(
			idx,
			_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p21)) > -1) ? _elm_lang$core$Maybe$Just(
			A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, _p20._3)) : _elm_lang$core$Maybe$Just(
			A3(_Skinney$elm_array_exploration$Array_Hamt$getHelp, _p20._1, idx, _p20._2)));
	});
var _Skinney$elm_array_exploration$Array_Hamt$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (treeEnd >>> shift);
			var _p22 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			if (_p22.ctor === 'SubTree') {
				var _v15 = shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_v16 = end,
					_v17 = treeEnd,
					_v18 = _p22._0;
				shift = _v15;
				end = _v16;
				treeEnd = _v17;
				tree = _v18;
				continue fetchNewTail;
			} else {
				return A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, _Skinney$elm_array_exploration$Array_Hamt$bitMask & end, _p22._0);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(oldShift, newShift) < 1) {
				return tree;
			} else {
				var _p23 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, 0, tree);
				if (_p23.ctor === 'SubTree') {
					var _v20 = oldShift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
						_v21 = newShift,
						_v22 = _p23._0;
					oldShift = _v20;
					newShift = _v21;
					tree = _v22;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$builderFromArray = function (_p24) {
	var _p25 = _p24;
	var helper = F2(
		function (node, acc) {
			var _p26 = node;
			if (_p26.ctor === 'SubTree') {
				return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, acc, _p26._0);
			} else {
				return {ctor: '::', _0: node, _1: acc};
			}
		});
	return {
		tail: _p25._3,
		nodeList: A3(
			_Skinney$elm_array_exploration$Array_JsArray$foldl,
			helper,
			{ctor: '[]'},
			_p25._2),
		nodeListSize: (_p25._0 / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0
	};
};
var _Skinney$elm_array_exploration$Array_Hamt$Builder = F3(
	function (a, b, c) {
		return {tail: a, nodeList: b, nodeListSize: c};
	});
var _Skinney$elm_array_exploration$Array_Hamt$Array = F4(
	function (a, b, c, d) {
		return {ctor: 'Array', _0: a, _1: b, _2: c, _3: d};
	});
var _Skinney$elm_array_exploration$Array_Hamt$empty = A4(_Skinney$elm_array_exploration$Array_Hamt$Array, 0, _Skinney$elm_array_exploration$Array_Hamt$shiftStep, _Skinney$elm_array_exploration$Array_JsArray$empty, _Skinney$elm_array_exploration$Array_JsArray$empty);
var _Skinney$elm_array_exploration$Array_Hamt$Leaf = function (a) {
	return {ctor: 'Leaf', _0: a};
};
var _Skinney$elm_array_exploration$Array_Hamt$appendHelp = F2(
	function (tail, builder) {
		var tailLen = _Skinney$elm_array_exploration$Array_JsArray$length(tail);
		var leftOver = A2(
			_elm_lang$core$Basics$max,
			0,
			(_Skinney$elm_array_exploration$Array_JsArray$length(builder.tail) + tailLen) - _Skinney$elm_array_exploration$Array_Hamt$branchFactor);
		var appended = A3(_Skinney$elm_array_exploration$Array_JsArray$appendN, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, builder.tail, tail);
		var appendedLen = _Skinney$elm_array_exploration$Array_JsArray$length(appended);
		return _elm_lang$core$Native_Utils.eq(appendedLen, _Skinney$elm_array_exploration$Array_Hamt$branchFactor) ? {
			tail: A3(_Skinney$elm_array_exploration$Array_JsArray$slice, tailLen - leftOver, tailLen, tail),
			nodeList: {
				ctor: '::',
				_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(appended),
				_1: builder.nodeList
			},
			nodeListSize: builder.nodeListSize + 1
		} : {tail: appended, nodeList: builder.nodeList, nodeListSize: builder.nodeListSize};
	});
var _Skinney$elm_array_exploration$Array_Hamt$SubTree = function (a) {
	return {ctor: 'SubTree', _0: a};
};
var _Skinney$elm_array_exploration$Array_Hamt$setHelp = F4(
	function (shift, idx, val, tree) {
		var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
		var _p27 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
		if (_p27.ctor === 'SubTree') {
			var newSub = A4(_Skinney$elm_array_exploration$Array_Hamt$setHelp, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, val, _p27._0);
			return A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				pos,
				_Skinney$elm_array_exploration$Array_Hamt$SubTree(newSub),
				tree);
		} else {
			var newLeaf = A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, val, _p27._0);
			return A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				pos,
				_Skinney$elm_array_exploration$Array_Hamt$Leaf(newLeaf),
				tree);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$set = F3(
	function (idx, val, _p28) {
		var _p29 = _p28;
		var _p33 = _p29._2;
		var _p32 = _p29._3;
		var _p31 = _p29._1;
		var _p30 = _p29._0;
		return ((_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(idx, _p30) > -1)) ? _p29 : ((_elm_lang$core$Native_Utils.cmp(
			idx,
			_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p30)) > -1) ? A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p30,
			_p31,
			_p33,
			A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, _Skinney$elm_array_exploration$Array_Hamt$bitMask & idx, val, _p32)) : A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p30,
			_p31,
			A4(_Skinney$elm_array_exploration$Array_Hamt$setHelp, _p31, idx, val, _p33),
			_p32));
	});
var _Skinney$elm_array_exploration$Array_Hamt$pushTailHelp = F4(
	function (shift, idx, tail, tree) {
		var pos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (idx >>> shift);
		if (_elm_lang$core$Native_Utils.cmp(
			pos,
			_Skinney$elm_array_exploration$Array_JsArray$length(tree)) > -1) {
			if (_elm_lang$core$Native_Utils.eq(shift, 5)) {
				return A2(
					_Skinney$elm_array_exploration$Array_JsArray$push,
					_Skinney$elm_array_exploration$Array_Hamt$Leaf(tail),
					tree);
			} else {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(_Skinney$elm_array_exploration$Array_Hamt$pushTailHelp, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, tail, _Skinney$elm_array_exploration$Array_JsArray$empty));
				return A2(_Skinney$elm_array_exploration$Array_JsArray$push, newSub, tree);
			}
		} else {
			var val = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, pos, tree);
			var _p34 = val;
			if (_p34.ctor === 'SubTree') {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(_Skinney$elm_array_exploration$Array_Hamt$pushTailHelp, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, idx, tail, _p34._0));
				return A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A4(
						_Skinney$elm_array_exploration$Array_Hamt$pushTailHelp,
						shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep,
						idx,
						tail,
						_Skinney$elm_array_exploration$Array_JsArray$singleton(val)));
				return A3(_Skinney$elm_array_exploration$Array_JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$push = F2(
	function (a, _p35) {
		var _p36 = _p35;
		var _p39 = _p36._2;
		var _p38 = _p36._1;
		var _p37 = _p36._0;
		var newLen = _p37 + 1;
		var modifiedTail = A2(_Skinney$elm_array_exploration$Array_JsArray$push, a, _p36._3);
		var tailLen = _Skinney$elm_array_exploration$Array_JsArray$length(modifiedTail);
		if (_elm_lang$core$Native_Utils.eq(tailLen, _Skinney$elm_array_exploration$Array_Hamt$branchFactor)) {
			var overflow = _elm_lang$core$Native_Utils.cmp(newLen >>> _Skinney$elm_array_exploration$Array_Hamt$shiftStep, 1 << _p38) > 0;
			if (overflow) {
				var spacyTree = _Skinney$elm_array_exploration$Array_JsArray$singleton(
					_Skinney$elm_array_exploration$Array_Hamt$SubTree(_p39));
				var newShift = _p38 + _Skinney$elm_array_exploration$Array_Hamt$shiftStep;
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					newLen,
					newShift,
					A4(_Skinney$elm_array_exploration$Array_Hamt$pushTailHelp, newShift, _p37, modifiedTail, spacyTree),
					_Skinney$elm_array_exploration$Array_JsArray$empty);
			} else {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					newLen,
					_p38,
					A4(_Skinney$elm_array_exploration$Array_Hamt$pushTailHelp, _p38, _p37, modifiedTail, _p39),
					_Skinney$elm_array_exploration$Array_JsArray$empty);
			}
		} else {
			return A4(_Skinney$elm_array_exploration$Array_Hamt$Array, newLen, _p38, _p39, modifiedTail);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$map = F2(
	function (f, _p40) {
		var _p41 = _p40;
		var helper = function (i) {
			var _p42 = i;
			if (_p42.ctor === 'SubTree') {
				return _Skinney$elm_array_exploration$Array_Hamt$SubTree(
					A2(_Skinney$elm_array_exploration$Array_JsArray$map, helper, _p42._0));
			} else {
				return _Skinney$elm_array_exploration$Array_Hamt$Leaf(
					A2(_Skinney$elm_array_exploration$Array_JsArray$map, f, _p42._0));
			}
		};
		return A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_p41._0,
			_p41._1,
			A2(_Skinney$elm_array_exploration$Array_JsArray$map, helper, _p41._2),
			A2(_Skinney$elm_array_exploration$Array_JsArray$map, f, _p41._3));
	});
var _Skinney$elm_array_exploration$Array_Hamt$toString = function (array) {
	var content = A2(
		_elm_lang$core$String$join,
		',',
		_Skinney$elm_array_exploration$Array_Hamt$toList(
			A2(_Skinney$elm_array_exploration$Array_Hamt$map, _elm_lang$core$Basics$toString, array)));
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'Array [',
		A2(_elm_lang$core$Basics_ops['++'], content, ']'));
};
var _Skinney$elm_array_exploration$Array_Hamt$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = _Skinney$elm_array_exploration$Array_Hamt$bitMask & (endIdx >>> shift);
		var _p43 = A2(_Skinney$elm_array_exploration$Array_JsArray$unsafeGet, lastPos, tree);
		if (_p43.ctor === 'SubTree') {
			var newSub = A3(_Skinney$elm_array_exploration$Array_Hamt$sliceTree, shift - _Skinney$elm_array_exploration$Array_Hamt$shiftStep, endIdx, _p43._0);
			return _elm_lang$core$Native_Utils.eq(
				_Skinney$elm_array_exploration$Array_JsArray$length(newSub),
				0) ? A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos, tree) : A3(
				_Skinney$elm_array_exploration$Array_JsArray$unsafeSet,
				lastPos,
				_Skinney$elm_array_exploration$Array_Hamt$SubTree(newSub),
				A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, lastPos, tree);
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$sliceRight = F2(
	function (end, _p44) {
		var _p45 = _p44;
		var _p48 = _p45._2;
		var _p47 = _p45._1;
		var _p46 = _p45._0;
		if (_elm_lang$core$Native_Utils.eq(end, _p46)) {
			return _p45;
		} else {
			if (_elm_lang$core$Native_Utils.cmp(
				end,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p46)) > -1) {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					end,
					_p47,
					_p48,
					A3(_Skinney$elm_array_exploration$Array_JsArray$slice, 0, _Skinney$elm_array_exploration$Array_Hamt$bitMask & end, _p45._3));
			} else {
				var endIdx = _Skinney$elm_array_exploration$Array_Hamt$tailIndex(end);
				var depth = _elm_lang$core$Basics$floor(
					A2(
						_elm_lang$core$Basics$logBase,
						_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor),
						_elm_lang$core$Basics$toFloat(endIdx - 1)));
				var newShift = A2(_elm_lang$core$Basics$max, 5, depth * _Skinney$elm_array_exploration$Array_Hamt$shiftStep);
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					end,
					newShift,
					A3(
						_Skinney$elm_array_exploration$Array_Hamt$hoistTree,
						_p47,
						newShift,
						A3(_Skinney$elm_array_exploration$Array_Hamt$sliceTree, _p47, endIdx, _p48)),
					A4(_Skinney$elm_array_exploration$Array_Hamt$fetchNewTail, _p47, end, endIdx, _p48));
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _p49 = A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, nodes);
			var node = _p49._0;
			var remainingNodes = _p49._1;
			var newAcc = {
				ctor: '::',
				_0: _Skinney$elm_array_exploration$Array_Hamt$SubTree(node),
				_1: acc
			};
			var _p50 = remainingNodes;
			if (_p50.ctor === '[]') {
				return _elm_lang$core$List$reverse(newAcc);
			} else {
				var _v34 = remainingNodes,
					_v35 = newAcc;
				nodes = _v34;
				acc = _v35;
				continue compressNodes;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = _elm_lang$core$Basics$ceiling(
				_elm_lang$core$Basics$toFloat(nodeListSize) / _elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor));
			if (_elm_lang$core$Native_Utils.eq(newNodeSize, 1)) {
				return _elm_lang$core$Tuple$first(
					A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, nodeList));
			} else {
				var _v36 = A2(
					_Skinney$elm_array_exploration$Array_Hamt$compressNodes,
					nodeList,
					{ctor: '[]'}),
					_v37 = newNodeSize;
				nodeList = _v36;
				nodeListSize = _v37;
				continue treeFromBuilder;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$builderToArray = function (builder) {
	if (_elm_lang$core$Native_Utils.eq(builder.nodeListSize, 0)) {
		return A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_Skinney$elm_array_exploration$Array_JsArray$length(builder.tail),
			_Skinney$elm_array_exploration$Array_Hamt$shiftStep,
			_Skinney$elm_array_exploration$Array_JsArray$empty,
			builder.tail);
	} else {
		var tree = A2(
			_Skinney$elm_array_exploration$Array_Hamt$treeFromBuilder,
			_elm_lang$core$List$reverse(builder.nodeList),
			builder.nodeListSize);
		var treeLen = builder.nodeListSize * _Skinney$elm_array_exploration$Array_Hamt$branchFactor;
		var depth = _elm_lang$core$Basics$floor(
			A2(
				_elm_lang$core$Basics$logBase,
				_elm_lang$core$Basics$toFloat(_Skinney$elm_array_exploration$Array_Hamt$branchFactor),
				_elm_lang$core$Basics$toFloat(treeLen - 1)));
		return A4(
			_Skinney$elm_array_exploration$Array_Hamt$Array,
			_Skinney$elm_array_exploration$Array_JsArray$length(builder.tail) + treeLen,
			A2(_elm_lang$core$Basics$max, 5, depth * _Skinney$elm_array_exploration$Array_Hamt$shiftStep),
			tree,
			builder.tail);
	}
};
var _Skinney$elm_array_exploration$Array_Hamt$initializeHelp = F5(
	function (fromIndex, toIndex, length, fn, nodeList) {
		initializeHelp:
		while (true) {
			var nodeSize = A2(_elm_lang$core$Basics$min, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, toIndex - fromIndex);
			var node = A3(_Skinney$elm_array_exploration$Array_JsArray$initialize, nodeSize, fromIndex, fn);
			if (_elm_lang$core$Native_Utils.cmp(nodeSize, _Skinney$elm_array_exploration$Array_Hamt$branchFactor) < 0) {
				return _Skinney$elm_array_exploration$Array_Hamt$builderToArray(
					{tail: node, nodeList: nodeList, nodeListSize: (length / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0});
			} else {
				var _v38 = fromIndex + nodeSize,
					_v39 = toIndex,
					_v40 = length,
					_v41 = fn,
					_v42 = {
					ctor: '::',
					_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(node),
					_1: nodeList
				};
				fromIndex = _v38;
				toIndex = _v39;
				length = _v40;
				fn = _v41;
				nodeList = _v42;
				continue initializeHelp;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$initialize = F2(
	function (length, fn) {
		return (_elm_lang$core$Native_Utils.cmp(length, 0) < 1) ? _Skinney$elm_array_exploration$Array_Hamt$empty : A5(
			_Skinney$elm_array_exploration$Array_Hamt$initializeHelp,
			0,
			length,
			length,
			fn,
			{ctor: '[]'});
	});
var _Skinney$elm_array_exploration$Array_Hamt$repeat = F2(
	function (n, e) {
		return A2(
			_Skinney$elm_array_exploration$Array_Hamt$initialize,
			n,
			function (_p51) {
				return e;
			});
	});
var _Skinney$elm_array_exploration$Array_Hamt$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _p52 = A2(_Skinney$elm_array_exploration$Array_JsArray$initializeFromList, _Skinney$elm_array_exploration$Array_Hamt$branchFactor, list);
			var jsArray = _p52._0;
			var remainingItems = _p52._1;
			if (_elm_lang$core$Native_Utils.cmp(
				_Skinney$elm_array_exploration$Array_JsArray$length(jsArray),
				_Skinney$elm_array_exploration$Array_Hamt$branchFactor) < 0) {
				return _Skinney$elm_array_exploration$Array_Hamt$builderToArray(
					{tail: jsArray, nodeList: nodeList, nodeListSize: nodeListSize});
			} else {
				var _v43 = remainingItems,
					_v44 = {
					ctor: '::',
					_0: _Skinney$elm_array_exploration$Array_Hamt$Leaf(jsArray),
					_1: nodeList
				},
					_v45 = nodeListSize + 1;
				list = _v43;
				nodeList = _v44;
				nodeListSize = _v45;
				continue fromListHelp;
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$fromList = function (ls) {
	var _p53 = ls;
	if (_p53.ctor === '[]') {
		return _Skinney$elm_array_exploration$Array_Hamt$empty;
	} else {
		return A3(
			_Skinney$elm_array_exploration$Array_Hamt$fromListHelp,
			ls,
			{ctor: '[]'},
			0);
	}
};
var _Skinney$elm_array_exploration$Array_Hamt$filter = F2(
	function (f, arr) {
		var helper = F2(
			function (n, acc) {
				return f(n) ? {ctor: '::', _0: n, _1: acc} : acc;
			});
		return _Skinney$elm_array_exploration$Array_Hamt$fromList(
			A3(
				_Skinney$elm_array_exploration$Array_Hamt$foldr,
				helper,
				{ctor: '[]'},
				arr));
	});
var _Skinney$elm_array_exploration$Array_Hamt$indexedMap = F2(
	function (f, _p54) {
		var _p55 = _p54;
		var initialBuilder = {
			tail: A3(
				_Skinney$elm_array_exploration$Array_JsArray$indexedMap,
				f,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p55._0),
				_p55._3),
			nodeList: {ctor: '[]'},
			nodeListSize: 0
		};
		var helper = F2(
			function (node, builder) {
				var _p56 = node;
				if (_p56.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, builder, _p56._0);
				} else {
					var offset = builder.nodeListSize * _Skinney$elm_array_exploration$Array_Hamt$branchFactor;
					var mappedLeaf = _Skinney$elm_array_exploration$Array_Hamt$Leaf(
						A3(_Skinney$elm_array_exploration$Array_JsArray$indexedMap, f, offset, _p56._0));
					return {
						tail: builder.tail,
						nodeList: {ctor: '::', _0: mappedLeaf, _1: builder.nodeList},
						nodeListSize: builder.nodeListSize + 1
					};
				}
			});
		return _Skinney$elm_array_exploration$Array_Hamt$builderToArray(
			A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, helper, initialBuilder, _p55._2));
	});
var _Skinney$elm_array_exploration$Array_Hamt$append = F2(
	function (a, _p57) {
		var _p58 = _p57;
		var foldHelper = F2(
			function (node, builder) {
				var _p59 = node;
				if (_p59.ctor === 'SubTree') {
					return A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, builder, _p59._0);
				} else {
					return A2(_Skinney$elm_array_exploration$Array_Hamt$appendHelp, _p59._0, builder);
				}
			});
		var builder = _Skinney$elm_array_exploration$Array_Hamt$builderFromArray(a);
		return _Skinney$elm_array_exploration$Array_Hamt$builderToArray(
			A2(
				_Skinney$elm_array_exploration$Array_Hamt$appendHelp,
				_p58._3,
				A3(_Skinney$elm_array_exploration$Array_JsArray$foldl, foldHelper, builder, _p58._2)));
	});
var _Skinney$elm_array_exploration$Array_Hamt$sliceLeft = F2(
	function (from, _p60) {
		var _p61 = _p60;
		var _p66 = _p61._3;
		var _p65 = _p61._0;
		if (_elm_lang$core$Native_Utils.eq(from, 0)) {
			return _p61;
		} else {
			if (_elm_lang$core$Native_Utils.cmp(
				from,
				_Skinney$elm_array_exploration$Array_Hamt$tailIndex(_p65)) > -1) {
				return A4(
					_Skinney$elm_array_exploration$Array_Hamt$Array,
					_p65 - from,
					_Skinney$elm_array_exploration$Array_Hamt$shiftStep,
					_Skinney$elm_array_exploration$Array_JsArray$empty,
					A3(
						_Skinney$elm_array_exploration$Array_JsArray$slice,
						from,
						_Skinney$elm_array_exploration$Array_JsArray$length(_p66),
						_p66));
			} else {
				var skipNodes = (from / _Skinney$elm_array_exploration$Array_Hamt$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						var _p62 = node;
						if (_p62.ctor === 'SubTree') {
							return A3(_Skinney$elm_array_exploration$Array_JsArray$foldr, helper, acc, _p62._0);
						} else {
							return {ctor: '::', _0: _p62._0, _1: acc};
						}
					});
				var leafNodes = A3(
					_Skinney$elm_array_exploration$Array_JsArray$foldr,
					helper,
					{
						ctor: '::',
						_0: _p66,
						_1: {ctor: '[]'}
					},
					_p61._2);
				var nodesToInsert = A2(_elm_lang$core$List$drop, skipNodes, leafNodes);
				var _p63 = nodesToInsert;
				if (_p63.ctor === '[]') {
					return _Skinney$elm_array_exploration$Array_Hamt$empty;
				} else {
					var _p64 = _p63._0;
					var firstSlice = from - (skipNodes * _Skinney$elm_array_exploration$Array_Hamt$branchFactor);
					var initialBuilder = {
						tail: A3(
							_Skinney$elm_array_exploration$Array_JsArray$slice,
							firstSlice,
							_Skinney$elm_array_exploration$Array_JsArray$length(_p64),
							_p64),
						nodeList: {ctor: '[]'},
						nodeListSize: 0
					};
					return _Skinney$elm_array_exploration$Array_Hamt$builderToArray(
						A3(_elm_lang$core$List$foldl, _Skinney$elm_array_exploration$Array_Hamt$appendHelp, initialBuilder, _p63._1));
				}
			}
		}
	});
var _Skinney$elm_array_exploration$Array_Hamt$slice = F3(
	function (from, to, arr) {
		var correctTo = A2(_Skinney$elm_array_exploration$Array_Hamt$translateIndex, to, arr);
		var correctFrom = A2(_Skinney$elm_array_exploration$Array_Hamt$translateIndex, from, arr);
		return (_elm_lang$core$Native_Utils.cmp(correctFrom, correctTo) > 0) ? _Skinney$elm_array_exploration$Array_Hamt$empty : A2(
			_Skinney$elm_array_exploration$Array_Hamt$sliceLeft,
			correctFrom,
			A2(_Skinney$elm_array_exploration$Array_Hamt$sliceRight, correctTo, arr));
	});

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _debois$elm_dom$DOM$className = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'className',
		_1: {ctor: '[]'}
	},
	_elm_lang$core$Json_Decode$string);
var _debois$elm_dom$DOM$scrollTop = A2(_elm_lang$core$Json_Decode$field, 'scrollTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$scrollLeft = A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetTop = A2(_elm_lang$core$Json_Decode$field, 'offsetTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetLeft = A2(_elm_lang$core$Json_Decode$field, 'offsetLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetHeight = A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetWidth = A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$childNodes = function (decoder) {
	var loop = F2(
		function (idx, xs) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (_p0) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$succeed(xs),
						A2(
							_elm_lang$core$Maybe$map,
							function (x) {
								return A2(
									loop,
									idx + 1,
									{ctor: '::', _0: x, _1: xs});
							},
							_p0));
				},
				_elm_lang$core$Json_Decode$maybe(
					A2(
						_elm_lang$core$Json_Decode$field,
						_elm_lang$core$Basics$toString(idx),
						decoder)));
		});
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$List$reverse,
		A2(
			_elm_lang$core$Json_Decode$field,
			'childNodes',
			A2(
				loop,
				0,
				{ctor: '[]'})));
};
var _debois$elm_dom$DOM$childNode = function (idx) {
	return _elm_lang$core$Json_Decode$at(
		{
			ctor: '::',
			_0: 'childNodes',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(idx),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'parentElement', decoder);
};
var _debois$elm_dom$DOM$previousSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'previousSibling', decoder);
};
var _debois$elm_dom$DOM$nextSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'nextSibling', decoder);
};
var _debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: A2(
					_elm_lang$core$Json_Decode$field,
					'offsetParent',
					_elm_lang$core$Json_Decode$null(x)),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$field, 'offsetParent', decoder),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (_p1) {
				var _p2 = _p1;
				var _p4 = _p2._1;
				var _p3 = _p2._0;
				return A2(
					_debois$elm_dom$DOM$offsetParent,
					{ctor: '_Tuple2', _0: _p3, _1: _p4},
					A2(_debois$elm_dom$DOM$position, _p3, _p4));
			},
			A5(
				_elm_lang$core$Json_Decode$map4,
				F4(
					function (scrollLeft, scrollTop, offsetLeft, offsetTop) {
						return {ctor: '_Tuple2', _0: (x + offsetLeft) - scrollLeft, _1: (y + offsetTop) - scrollTop};
					}),
				_debois$elm_dom$DOM$scrollLeft,
				_debois$elm_dom$DOM$scrollTop,
				_debois$elm_dom$DOM$offsetLeft,
				_debois$elm_dom$DOM$offsetTop));
	});
var _debois$elm_dom$DOM$boundingClientRect = A4(
	_elm_lang$core$Json_Decode$map3,
	F3(
		function (_p5, width, height) {
			var _p6 = _p5;
			return {top: _p6._1, left: _p6._0, width: width, height: height};
		}),
	A2(_debois$elm_dom$DOM$position, 0, 0),
	_debois$elm_dom$DOM$offsetWidth,
	_debois$elm_dom$DOM$offsetHeight);
var _debois$elm_dom$DOM$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'target', decoder);
};
var _debois$elm_dom$DOM$Rectangle = F4(
	function (a, b, c, d) {
		return {top: a, left: b, width: c, height: d};
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$singleton = function (x) {
	return {
		ctor: '::',
		_0: x,
		_1: {ctor: '[]'}
	};
};
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

var _elm_lang$animation_frame$Native_AnimationFrame = function()
{

function create()
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = requestAnimationFrame(function() {
			callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
		});

		return function() {
			cancelAnimationFrame(id);
		};
	});
}

return {
	create: create
};

}();

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$animation_frame$AnimationFrame$rAF = _elm_lang$animation_frame$Native_AnimationFrame.create(
	{ctor: '_Tuple0'});
var _elm_lang$animation_frame$AnimationFrame$subscription = _elm_lang$core$Native_Platform.leaf('AnimationFrame');
var _elm_lang$animation_frame$AnimationFrame$State = F3(
	function (a, b, c) {
		return {subs: a, request: b, oldTime: c};
	});
var _elm_lang$animation_frame$AnimationFrame$init = _elm_lang$core$Task$succeed(
	A3(
		_elm_lang$animation_frame$AnimationFrame$State,
		{ctor: '[]'},
		_elm_lang$core$Maybe$Nothing,
		0));
var _elm_lang$animation_frame$AnimationFrame$onEffects = F3(
	function (router, subs, _p0) {
		var _p1 = _p0;
		var _p5 = _p1.request;
		var _p4 = _p1.oldTime;
		var _p2 = {ctor: '_Tuple2', _0: _p5, _1: subs};
		if (_p2._0.ctor === 'Nothing') {
			if (_p2._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(
					A3(
						_elm_lang$animation_frame$AnimationFrame$State,
						{ctor: '[]'},
						_elm_lang$core$Maybe$Nothing,
						_p4));
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (time) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$animation_frame$AnimationFrame$State,
										subs,
										_elm_lang$core$Maybe$Just(pid),
										time));
							},
							_elm_lang$core$Time$now);
					},
					_elm_lang$core$Process$spawn(
						A2(
							_elm_lang$core$Task$andThen,
							_elm_lang$core$Platform$sendToSelf(router),
							_elm_lang$animation_frame$AnimationFrame$rAF)));
			}
		} else {
			if (_p2._1.ctor === '[]') {
				return A2(
					_elm_lang$core$Task$andThen,
					function (_p3) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$animation_frame$AnimationFrame$State,
								{ctor: '[]'},
								_elm_lang$core$Maybe$Nothing,
								_p4));
					},
					_elm_lang$core$Process$kill(_p2._0._0));
			} else {
				return _elm_lang$core$Task$succeed(
					A3(_elm_lang$animation_frame$AnimationFrame$State, subs, _p5, _p4));
			}
		}
	});
var _elm_lang$animation_frame$AnimationFrame$onSelfMsg = F3(
	function (router, newTime, _p6) {
		var _p7 = _p6;
		var _p10 = _p7.subs;
		var diff = newTime - _p7.oldTime;
		var send = function (sub) {
			var _p8 = sub;
			if (_p8.ctor === 'Time') {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p8._0(newTime));
			} else {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p8._0(diff));
			}
		};
		return A2(
			_elm_lang$core$Task$andThen,
			function (pid) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (_p9) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$animation_frame$AnimationFrame$State,
								_p10,
								_elm_lang$core$Maybe$Just(pid),
								newTime));
					},
					_elm_lang$core$Task$sequence(
						A2(_elm_lang$core$List$map, send, _p10)));
			},
			_elm_lang$core$Process$spawn(
				A2(
					_elm_lang$core$Task$andThen,
					_elm_lang$core$Platform$sendToSelf(router),
					_elm_lang$animation_frame$AnimationFrame$rAF)));
	});
var _elm_lang$animation_frame$AnimationFrame$Diff = function (a) {
	return {ctor: 'Diff', _0: a};
};
var _elm_lang$animation_frame$AnimationFrame$diffs = function (tagger) {
	return _elm_lang$animation_frame$AnimationFrame$subscription(
		_elm_lang$animation_frame$AnimationFrame$Diff(tagger));
};
var _elm_lang$animation_frame$AnimationFrame$Time = function (a) {
	return {ctor: 'Time', _0: a};
};
var _elm_lang$animation_frame$AnimationFrame$times = function (tagger) {
	return _elm_lang$animation_frame$AnimationFrame$subscription(
		_elm_lang$animation_frame$AnimationFrame$Time(tagger));
};
var _elm_lang$animation_frame$AnimationFrame$subMap = F2(
	function (func, sub) {
		var _p11 = sub;
		if (_p11.ctor === 'Time') {
			return _elm_lang$animation_frame$AnimationFrame$Time(
				function (_p12) {
					return func(
						_p11._0(_p12));
				});
		} else {
			return _elm_lang$animation_frame$AnimationFrame$Diff(
				function (_p13) {
					return func(
						_p11._0(_p13));
				});
		}
	});
_elm_lang$core$Native_Platform.effectManagers['AnimationFrame'] = {pkg: 'elm-lang/animation-frame', init: _elm_lang$animation_frame$AnimationFrame$init, onEffects: _elm_lang$animation_frame$AnimationFrame$onEffects, onSelfMsg: _elm_lang$animation_frame$AnimationFrame$onSelfMsg, tag: 'sub', subMap: _elm_lang$animation_frame$AnimationFrame$subMap};

var _elm_lang$core$Color$fmod = F2(
	function (f, n) {
		var integer = _elm_lang$core$Basics$floor(f);
		return (_elm_lang$core$Basics$toFloat(
			A2(_elm_lang$core$Basics_ops['%'], integer, n)) + f) - _elm_lang$core$Basics$toFloat(integer);
	});
var _elm_lang$core$Color$rgbToHsl = F3(
	function (red, green, blue) {
		var b = _elm_lang$core$Basics$toFloat(blue) / 255;
		var g = _elm_lang$core$Basics$toFloat(green) / 255;
		var r = _elm_lang$core$Basics$toFloat(red) / 255;
		var cMax = A2(
			_elm_lang$core$Basics$max,
			A2(_elm_lang$core$Basics$max, r, g),
			b);
		var cMin = A2(
			_elm_lang$core$Basics$min,
			A2(_elm_lang$core$Basics$min, r, g),
			b);
		var c = cMax - cMin;
		var lightness = (cMax + cMin) / 2;
		var saturation = _elm_lang$core$Native_Utils.eq(lightness, 0) ? 0 : (c / (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)));
		var hue = _elm_lang$core$Basics$degrees(60) * (_elm_lang$core$Native_Utils.eq(cMax, r) ? A2(_elm_lang$core$Color$fmod, (g - b) / c, 6) : (_elm_lang$core$Native_Utils.eq(cMax, g) ? (((b - r) / c) + 2) : (((r - g) / c) + 4)));
		return {ctor: '_Tuple3', _0: hue, _1: saturation, _2: lightness};
	});
var _elm_lang$core$Color$hslToRgb = F3(
	function (hue, saturation, lightness) {
		var normHue = hue / _elm_lang$core$Basics$degrees(60);
		var chroma = (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)) * saturation;
		var x = chroma * (1 - _elm_lang$core$Basics$abs(
			A2(_elm_lang$core$Color$fmod, normHue, 2) - 1));
		var _p0 = (_elm_lang$core$Native_Utils.cmp(normHue, 0) < 0) ? {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 1) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: x, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 2) < 0) ? {ctor: '_Tuple3', _0: x, _1: chroma, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 3) < 0) ? {ctor: '_Tuple3', _0: 0, _1: chroma, _2: x} : ((_elm_lang$core$Native_Utils.cmp(normHue, 4) < 0) ? {ctor: '_Tuple3', _0: 0, _1: x, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 5) < 0) ? {ctor: '_Tuple3', _0: x, _1: 0, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 6) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: 0, _2: x} : {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0}))))));
		var r = _p0._0;
		var g = _p0._1;
		var b = _p0._2;
		var m = lightness - (chroma / 2);
		return {ctor: '_Tuple3', _0: r + m, _1: g + m, _2: b + m};
	});
var _elm_lang$core$Color$toRgb = function (color) {
	var _p1 = color;
	if (_p1.ctor === 'RGBA') {
		return {red: _p1._0, green: _p1._1, blue: _p1._2, alpha: _p1._3};
	} else {
		var _p2 = A3(_elm_lang$core$Color$hslToRgb, _p1._0, _p1._1, _p1._2);
		var r = _p2._0;
		var g = _p2._1;
		var b = _p2._2;
		return {
			red: _elm_lang$core$Basics$round(255 * r),
			green: _elm_lang$core$Basics$round(255 * g),
			blue: _elm_lang$core$Basics$round(255 * b),
			alpha: _p1._3
		};
	}
};
var _elm_lang$core$Color$toHsl = function (color) {
	var _p3 = color;
	if (_p3.ctor === 'HSLA') {
		return {hue: _p3._0, saturation: _p3._1, lightness: _p3._2, alpha: _p3._3};
	} else {
		var _p4 = A3(_elm_lang$core$Color$rgbToHsl, _p3._0, _p3._1, _p3._2);
		var h = _p4._0;
		var s = _p4._1;
		var l = _p4._2;
		return {hue: h, saturation: s, lightness: l, alpha: _p3._3};
	}
};
var _elm_lang$core$Color$HSLA = F4(
	function (a, b, c, d) {
		return {ctor: 'HSLA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$hsla = F4(
	function (hue, saturation, lightness, alpha) {
		return A4(
			_elm_lang$core$Color$HSLA,
			hue - _elm_lang$core$Basics$turns(
				_elm_lang$core$Basics$toFloat(
					_elm_lang$core$Basics$floor(hue / (2 * _elm_lang$core$Basics$pi)))),
			saturation,
			lightness,
			alpha);
	});
var _elm_lang$core$Color$hsl = F3(
	function (hue, saturation, lightness) {
		return A4(_elm_lang$core$Color$hsla, hue, saturation, lightness, 1);
	});
var _elm_lang$core$Color$complement = function (color) {
	var _p5 = color;
	if (_p5.ctor === 'HSLA') {
		return A4(
			_elm_lang$core$Color$hsla,
			_p5._0 + _elm_lang$core$Basics$degrees(180),
			_p5._1,
			_p5._2,
			_p5._3);
	} else {
		var _p6 = A3(_elm_lang$core$Color$rgbToHsl, _p5._0, _p5._1, _p5._2);
		var h = _p6._0;
		var s = _p6._1;
		var l = _p6._2;
		return A4(
			_elm_lang$core$Color$hsla,
			h + _elm_lang$core$Basics$degrees(180),
			s,
			l,
			_p5._3);
	}
};
var _elm_lang$core$Color$grayscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$greyscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$RGBA = F4(
	function (a, b, c, d) {
		return {ctor: 'RGBA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$rgba = _elm_lang$core$Color$RGBA;
var _elm_lang$core$Color$rgb = F3(
	function (r, g, b) {
		return A4(_elm_lang$core$Color$RGBA, r, g, b, 1);
	});
var _elm_lang$core$Color$lightRed = A4(_elm_lang$core$Color$RGBA, 239, 41, 41, 1);
var _elm_lang$core$Color$red = A4(_elm_lang$core$Color$RGBA, 204, 0, 0, 1);
var _elm_lang$core$Color$darkRed = A4(_elm_lang$core$Color$RGBA, 164, 0, 0, 1);
var _elm_lang$core$Color$lightOrange = A4(_elm_lang$core$Color$RGBA, 252, 175, 62, 1);
var _elm_lang$core$Color$orange = A4(_elm_lang$core$Color$RGBA, 245, 121, 0, 1);
var _elm_lang$core$Color$darkOrange = A4(_elm_lang$core$Color$RGBA, 206, 92, 0, 1);
var _elm_lang$core$Color$lightYellow = A4(_elm_lang$core$Color$RGBA, 255, 233, 79, 1);
var _elm_lang$core$Color$yellow = A4(_elm_lang$core$Color$RGBA, 237, 212, 0, 1);
var _elm_lang$core$Color$darkYellow = A4(_elm_lang$core$Color$RGBA, 196, 160, 0, 1);
var _elm_lang$core$Color$lightGreen = A4(_elm_lang$core$Color$RGBA, 138, 226, 52, 1);
var _elm_lang$core$Color$green = A4(_elm_lang$core$Color$RGBA, 115, 210, 22, 1);
var _elm_lang$core$Color$darkGreen = A4(_elm_lang$core$Color$RGBA, 78, 154, 6, 1);
var _elm_lang$core$Color$lightBlue = A4(_elm_lang$core$Color$RGBA, 114, 159, 207, 1);
var _elm_lang$core$Color$blue = A4(_elm_lang$core$Color$RGBA, 52, 101, 164, 1);
var _elm_lang$core$Color$darkBlue = A4(_elm_lang$core$Color$RGBA, 32, 74, 135, 1);
var _elm_lang$core$Color$lightPurple = A4(_elm_lang$core$Color$RGBA, 173, 127, 168, 1);
var _elm_lang$core$Color$purple = A4(_elm_lang$core$Color$RGBA, 117, 80, 123, 1);
var _elm_lang$core$Color$darkPurple = A4(_elm_lang$core$Color$RGBA, 92, 53, 102, 1);
var _elm_lang$core$Color$lightBrown = A4(_elm_lang$core$Color$RGBA, 233, 185, 110, 1);
var _elm_lang$core$Color$brown = A4(_elm_lang$core$Color$RGBA, 193, 125, 17, 1);
var _elm_lang$core$Color$darkBrown = A4(_elm_lang$core$Color$RGBA, 143, 89, 2, 1);
var _elm_lang$core$Color$black = A4(_elm_lang$core$Color$RGBA, 0, 0, 0, 1);
var _elm_lang$core$Color$white = A4(_elm_lang$core$Color$RGBA, 255, 255, 255, 1);
var _elm_lang$core$Color$lightGrey = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$grey = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGrey = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightGray = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$gray = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGray = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightCharcoal = A4(_elm_lang$core$Color$RGBA, 136, 138, 133, 1);
var _elm_lang$core$Color$charcoal = A4(_elm_lang$core$Color$RGBA, 85, 87, 83, 1);
var _elm_lang$core$Color$darkCharcoal = A4(_elm_lang$core$Color$RGBA, 46, 52, 54, 1);
var _elm_lang$core$Color$Radial = F5(
	function (a, b, c, d, e) {
		return {ctor: 'Radial', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Color$radial = _elm_lang$core$Color$Radial;
var _elm_lang$core$Color$Linear = F3(
	function (a, b, c) {
		return {ctor: 'Linear', _0: a, _1: b, _2: c};
	});
var _elm_lang$core$Color$linear = _elm_lang$core$Color$Linear;

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (!a.options === b.options)
	{
		if (a.stopPropagation !== b.stopPropagation || a.preventDefault !== b.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { callback(); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$Native_Debug = function() {


// IMPORT / EXPORT

function unsafeCoerce(value)
{
	return value;
}

var upload = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	var element = document.createElement('input');
	element.setAttribute('type', 'file');
	element.setAttribute('accept', 'text/json');
	element.style.display = 'none';
	element.addEventListener('change', function(event)
	{
		var fileReader = new FileReader();
		fileReader.onload = function(e)
		{
			callback(_elm_lang$core$Native_Scheduler.succeed(e.target.result));
		};
		fileReader.readAsText(event.target.files[0]);
		document.body.removeChild(element);
	});
	document.body.appendChild(element);
	element.click();
});

function download(historyLength, json)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = _elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
}


// POPOUT

function messageToString(value)
{
	switch (typeof value)
	{
		case 'boolean':
			return value ? 'True' : 'False';
		case 'number':
			return value + '';
		case 'string':
			return '"' + addSlashes(value, false) + '"';
	}
	if (value instanceof String)
	{
		return '\'' + addSlashes(value, true) + '\'';
	}
	if (typeof value !== 'object' || value === null || !('ctor' in value))
	{
		return '…';
	}

	var ctorStarter = value.ctor.substring(0, 5);
	if (ctorStarter === '_Tupl' || ctorStarter === '_Task')
	{
		return '…'
	}
	if (['_Array', '<decoder>', '_Process', '::', '[]', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.ctor) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.ctor;
		case 2:
			return value.ctor + ' ' + messageToString(value._0);
		default:
			return value.ctor + ' … ' + messageToString(value[keys[keys.length - 1]]);
	}
}


function primitive(str)
{
	return { ctor: 'Primitive', _0: str };
}


function init(value)
{
	var type = typeof value;

	if (type === 'boolean')
	{
		return {
			ctor: 'Constructor',
			_0: _elm_lang$core$Maybe$Just(value ? 'True' : 'False'),
			_1: true,
			_2: _elm_lang$core$Native_List.Nil
		};
	}

	if (type === 'number')
	{
		return primitive(value + '');
	}

	if (type === 'string')
	{
		return { ctor: 'S', _0: '"' + addSlashes(value, false) + '"' };
	}

	if (value instanceof String)
	{
		return { ctor: 'S', _0: "'" + addSlashes(value, true) + "'" };
	}

	if (value instanceof Date)
	{
		return primitive('<' + value.toString() + '>');
	}

	if (value === null)
	{
		return primitive('XXX');
	}

	if (type === 'object' && 'ctor' in value)
	{
		var ctor = value.ctor;

		if (ctor === '::' || ctor === '[]')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ListSeq'},
				_1: true,
				_2: A2(_elm_lang$core$List$map, init, value)
			};
		}

		if (ctor === 'Set_elm_builtin')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'SetSeq'},
				_1: true,
				_2: A3(_elm_lang$core$Set$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === 'RBNode_elm_builtin' || ctor == 'RBEmpty_elm_builtin')
		{
			return {
				ctor: 'Dictionary',
				_0: true,
				_1: A3(_elm_lang$core$Dict$foldr, initKeyValueCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === '_Array')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ArraySeq'},
				_1: true,
				_2: A3(_elm_lang$core$Array$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		var ctorStarter = value.ctor.substring(0, 5);
		if (ctorStarter === '_Task')
		{
			return primitive('<task>');
		}

		if (ctor === '<decoder>')
		{
			return primitive(ctor);
		}

		if (ctor === '_Process')
		{
			return primitive('<process>');
		}

		var list = _elm_lang$core$Native_List.Nil;
		for (var i in value)
		{
			if (i === 'ctor') continue;
			list = _elm_lang$core$Native_List.Cons(init(value[i]), list);
		}
		return {
			ctor: 'Constructor',
			_0: ctorStarter === '_Tupl' ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(ctor),
			_1: true,
			_2: _elm_lang$core$List$reverse(list)
		};
	}

	if (type === 'object')
	{
		var dict = _elm_lang$core$Dict$empty;
		for (var i in value)
		{
			dict = A3(_elm_lang$core$Dict$insert, i, init(value[i]), dict);
		}
		return { ctor: 'Record', _0: true, _1: dict };
	}

	return primitive('XXX');
}

var initCons = F2(initConsHelp);

function initConsHelp(value, list)
{
	return _elm_lang$core$Native_List.Cons(init(value), list);
}

var initKeyValueCons = F3(initKeyValueConsHelp);

function initKeyValueConsHelp(key, value, list)
{
	return _elm_lang$core$Native_List.Cons(
		_elm_lang$core$Native_Utils.Tuple2(init(key), init(value)),
		list
	);
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	upload: upload,
	download: F2(download),
	unsafeCoerce: unsafeCoerce,
	messageToString: messageToString,
	init: init
}

}();

var _elm_lang$virtual_dom$VirtualDom_Helpers$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom_Helpers$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$onClick = function (msg) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom_Helpers$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom_Helpers$id = _elm_lang$virtual_dom$VirtualDom_Helpers$attribute('id');
var _elm_lang$virtual_dom$VirtualDom_Helpers$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom_Helpers$class = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'className',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$href = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'href',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom_Helpers$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom_Helpers$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom_Helpers$div = _elm_lang$virtual_dom$VirtualDom_Helpers$node('div');
var _elm_lang$virtual_dom$VirtualDom_Helpers$span = _elm_lang$virtual_dom$VirtualDom_Helpers$node('span');
var _elm_lang$virtual_dom$VirtualDom_Helpers$a = _elm_lang$virtual_dom$VirtualDom_Helpers$node('a');
var _elm_lang$virtual_dom$VirtualDom_Helpers$h1 = _elm_lang$virtual_dom$VirtualDom_Helpers$node('h1');
var _elm_lang$virtual_dom$VirtualDom_Helpers$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom_Helpers$Property = {ctor: 'Property'};

var _elm_lang$virtual_dom$VirtualDom_Expando$purple = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(136, 19, 145)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$blue = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(28, 0, 207)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$red = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(196, 26, 22)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$leftPad = function (maybeKey) {
	var _p0 = maybeKey;
	if (_p0.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{ctor: '[]'});
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '4ch'},
				_1: {ctor: '[]'}
			});
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow = function (arrow) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$span,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'color', _1: '#777'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '2ch'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: '2ch'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'display', _1: 'inline-block'},
								_1: {ctor: '[]'}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(arrow),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Expando$lineStarter = F3(
	function (maybeKey, maybeIsClosed, description) {
		var arrow = function () {
			var _p1 = maybeIsClosed;
			if (_p1.ctor === 'Nothing') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('');
			} else {
				if (_p1._0 === true) {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▸');
				} else {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▾');
				}
			}
		}();
		var _p2 = maybeKey;
		if (_p2.ctor === 'Nothing') {
			return {ctor: '::', _0: arrow, _1: description};
		} else {
			return {
				ctor: '::',
				_0: arrow,
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p2._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
						_1: description
					}
				}
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord = F3(
	function (length, starter, entries) {
		var _p3 = entries;
		if (_p3.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 1,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p5 = _p3._0;
			var nextLength = (length + _elm_lang$core$String$length(_p5)) + 1;
			if (_elm_lang$core$Native_Utils.cmp(nextLength, 18) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 2,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('…}'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p4 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord, nextLength, ',', _p3._1);
				var finalLength = _p4._0;
				var otherNodes = _p4._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p5),
									_1: {ctor: '[]'}
								}),
							_1: otherNodes
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle = function (str) {
	return (_elm_lang$core$Native_Utils.cmp(
		_elm_lang$core$String$length(str),
		18) < 1) ? str : A2(
		_elm_lang$core$Basics_ops['++'],
		A2(_elm_lang$core$String$left, 8, str),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'...',
			A2(_elm_lang$core$String$right, 8, str)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp = function (str) {
	return {
		ctor: '_Tuple2',
		_0: _elm_lang$core$String$length(str),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
			_1: {ctor: '[]'}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$updateIndex = F3(
	function (n, func, list) {
		var _p6 = list;
		if (_p6.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p8 = _p6._1;
			var _p7 = _p6._0;
			return (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) ? {
				ctor: '::',
				_0: func(_p7),
				_1: _p8
			} : {
				ctor: '::',
				_0: _p7,
				_1: A3(_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex, n - 1, func, _p8)
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString = F2(
	function (n, seqType) {
		var _p9 = seqType;
		switch (_p9.ctor) {
			case 'ListSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'List(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			case 'SetSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Set(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Array(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny = function (value) {
	var _p10 = value;
	switch (_p10.ctor) {
		case 'S':
			var str = _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle(_p10._0);
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(str),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Primitive':
			var _p11 = _p10._0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(_p11),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p11),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Sequence':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
					_elm_lang$core$List$length(_p10._2),
					_p10._0));
		case 'Dictionary':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Dict(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(
							_elm_lang$core$List$length(_p10._1)),
						')')));
		case 'Record':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(_p10._1);
		default:
			if (_p10._2.ctor === '[]') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					A2(_elm_lang$core$Maybe$withDefault, 'Unit', _p10._0));
			} else {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					function () {
						var _p12 = _p10._0;
						if (_p12.ctor === 'Nothing') {
							return A2(
								_elm_lang$core$Basics_ops['++'],
								'Tuple(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(
										_elm_lang$core$List$length(_p10._2)),
									')'));
						} else {
							return A2(_elm_lang$core$Basics_ops['++'], _p12._0, ' …');
						}
					}());
			}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord = function (record) {
	return _elm_lang$core$Dict$isEmpty(record) ? {
		ctor: '_Tuple2',
		_0: 2,
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{}'),
			_1: {ctor: '[]'}
		}
	} : A3(
		_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp,
		0,
		'{ ',
		_elm_lang$core$Dict$toList(record));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp = F3(
	function (length, starter, entries) {
		var _p13 = entries;
		if (_p13.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 2,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' }'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p16 = _p13._0._0;
			var _p14 = _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p13._0._1);
			var valueLen = _p14._0;
			var valueNodes = _p14._1;
			var fieldLen = _elm_lang$core$String$length(_p16);
			var newLength = ((length + fieldLen) + valueLen) + 5;
			if (_elm_lang$core$Native_Utils.cmp(newLength, 60) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 4,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', … }'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p15 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp, newLength, ', ', _p13._1);
				var finalLength = _p15._0;
				var otherNodes = _p15._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p16),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$virtual_dom$VirtualDom_Helpers$span,
										{ctor: '[]'},
										valueNodes),
									_1: otherNodes
								}
							}
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny = function (value) {
	var _p17 = value;
	if (_p17.ctor === 'Record') {
		return A3(
			_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord,
			0,
			'{',
			_elm_lang$core$Dict$keys(_p17._1));
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny(value);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$Constructor = F3(
	function (a, b, c) {
		return {ctor: 'Constructor', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Record = F2(
	function (a, b) {
		return {ctor: 'Record', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Dictionary = F2(
	function (a, b) {
		return {ctor: 'Dictionary', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Sequence = F3(
	function (a, b, c) {
		return {ctor: 'Sequence', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$initHelp = F2(
	function (isOuter, expando) {
		var _p18 = expando;
		switch (_p18.ctor) {
			case 'S':
				return expando;
			case 'Primitive':
				return expando;
			case 'Sequence':
				var _p20 = _p18._0;
				var _p19 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
					_p20,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p19)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p19),
					8) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p20, false, _p19) : expando);
			case 'Dictionary':
				var _p23 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
					false,
					A2(
						_elm_lang$core$List$map,
						function (_p21) {
							var _p22 = _p21;
							return {
								ctor: '_Tuple2',
								_0: _p22._0,
								_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, _p22._1)
							};
						},
						_p23)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p23),
					8) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, false, _p23) : expando);
			case 'Record':
				var _p25 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Record,
					false,
					A2(
						_elm_lang$core$Dict$map,
						F2(
							function (_p24, v) {
								return A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, v);
							}),
						_p25)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Dict$size(_p25),
					4) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, false, _p25) : expando);
			default:
				var _p27 = _p18._0;
				var _p26 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
					_p27,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p26)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p26),
					4) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p27, false, _p26) : expando);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$init = function (value) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Expando$initHelp,
		true,
		_elm_lang$virtual_dom$Native_Debug.init(value));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp = F2(
	function (old, $new) {
		var _p28 = {ctor: '_Tuple2', _0: old, _1: $new};
		_v12_6:
		do {
			if (_p28.ctor === '_Tuple2') {
				switch (_p28._1.ctor) {
					case 'S':
						return $new;
					case 'Primitive':
						return $new;
					case 'Sequence':
						if (_p28._0.ctor === 'Sequence') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
					case 'Dictionary':
						if (_p28._0.ctor === 'Dictionary') {
							return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, _p28._0._0, _p28._1._1);
						} else {
							break _v12_6;
						}
					case 'Record':
						if (_p28._0.ctor === 'Record') {
							return A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$Record,
								_p28._0._0,
								A2(
									_elm_lang$core$Dict$map,
									_elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp(_p28._0._1),
									_p28._1._1));
						} else {
							break _v12_6;
						}
					default:
						if (_p28._0.ctor === 'Constructor') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
				}
			} else {
				break _v12_6;
			}
		} while(false);
		return $new;
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp = F3(
	function (oldDict, key, value) {
		var _p29 = A2(_elm_lang$core$Dict$get, key, oldDict);
		if (_p29.ctor === 'Nothing') {
			return value;
		} else {
			return A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p29._0, value);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp = F2(
	function (olds, news) {
		var _p30 = {ctor: '_Tuple2', _0: olds, _1: news};
		if (_p30._0.ctor === '[]') {
			return news;
		} else {
			if (_p30._1.ctor === '[]') {
				return news;
			} else {
				return {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p30._0._0, _p30._1._0),
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p30._0._1, _p30._1._1)
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$merge = F2(
	function (value, expando) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp,
			expando,
			_elm_lang$virtual_dom$Native_Debug.init(value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$update = F2(
	function (msg, value) {
		var _p31 = value;
		switch (_p31.ctor) {
			case 'S':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Primitive':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Sequence':
				var _p39 = _p31._2;
				var _p38 = _p31._0;
				var _p37 = _p31._1;
				var _p34 = msg;
				switch (_p34.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p38, !_p37, _p39);
					case 'Index':
						if (_p34._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p38,
								_p37,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p34._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p34._2),
									_p39));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 176, column: 7},
									end: {line: 188, column: 46}
								},
								_p34)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 176, column: 7},
								end: {line: 188, column: 46}
							},
							_p34)('No field on sequences');
				}
			case 'Dictionary':
				var _p51 = _p31._1;
				var _p50 = _p31._0;
				var _p40 = msg;
				switch (_p40.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, !_p50, _p51);
					case 'Index':
						var _p48 = _p40._2;
						var _p47 = _p40._1;
						var _p41 = _p40._0;
						switch (_p41.ctor) {
							case 'None':
								return _elm_lang$core$Native_Utils.crashCase(
									'VirtualDom.Expando',
									{
										start: {line: 196, column: 11},
										end: {line: 206, column: 81}
									},
									_p41)('must have redirect for dictionaries');
							case 'Key':
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p43) {
											var _p44 = _p43;
											return {
												ctor: '_Tuple2',
												_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p44._0),
												_1: _p44._1
											};
										},
										_p51));
							default:
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p45) {
											var _p46 = _p45;
											return {
												ctor: '_Tuple2',
												_0: _p46._0,
												_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p46._1)
											};
										},
										_p51));
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 191, column: 7},
								end: {line: 209, column: 50}
							},
							_p40)('no field for dictionaries');
				}
			case 'Record':
				var _p55 = _p31._1;
				var _p54 = _p31._0;
				var _p52 = msg;
				switch (_p52.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, !_p54, _p55);
					case 'Index':
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 212, column: 7},
								end: {line: 220, column: 77}
							},
							_p52)('No index for records');
					default:
						return A2(
							_elm_lang$virtual_dom$VirtualDom_Expando$Record,
							_p54,
							A3(
								_elm_lang$core$Dict$update,
								_p52._0,
								_elm_lang$virtual_dom$VirtualDom_Expando$updateField(_p52._1),
								_p55));
				}
			default:
				var _p61 = _p31._2;
				var _p60 = _p31._0;
				var _p59 = _p31._1;
				var _p56 = msg;
				switch (_p56.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p60, !_p59, _p61);
					case 'Index':
						if (_p56._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p60,
								_p59,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p56._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p56._2),
									_p61));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 223, column: 7},
									end: {line: 235, column: 50}
								},
								_p56)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 223, column: 7},
								end: {line: 235, column: 50}
							},
							_p56)('No field for constructors');
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$updateField = F2(
	function (msg, maybeExpando) {
		var _p62 = maybeExpando;
		if (_p62.ctor === 'Nothing') {
			return _elm_lang$core$Native_Utils.crashCase(
				'VirtualDom.Expando',
				{
					start: {line: 253, column: 3},
					end: {line: 258, column: 32}
				},
				_p62)('key does not exist');
		} else {
			return _elm_lang$core$Maybe$Just(
				A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, msg, _p62._0));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Primitive = function (a) {
	return {ctor: 'Primitive', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$S = function (a) {
	return {ctor: 'S', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$ArraySeq = {ctor: 'ArraySeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$SetSeq = {ctor: 'SetSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$ListSeq = {ctor: 'ListSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Field = F2(
	function (a, b) {
		return {ctor: 'Field', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Index = F3(
	function (a, b, c) {
		return {ctor: 'Index', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Toggle = {ctor: 'Toggle'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Value = {ctor: 'Value'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Key = {ctor: 'Key'};
var _elm_lang$virtual_dom$VirtualDom_Expando$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry = F2(
	function (index, value) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, index),
			A2(
				_elm_lang$virtual_dom$VirtualDom_Expando$view,
				_elm_lang$core$Maybe$Just(
					_elm_lang$core$Basics$toString(index)),
				value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$view = F2(
	function (maybeKey, expando) {
		var _p64 = expando;
		switch (_p64.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Sequence':
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewSequence, maybeKey, _p64._0, _p64._1, _p64._2);
			case 'Dictionary':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary, maybeKey, _p64._0, _p64._1);
			case 'Record':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewRecord, maybeKey, _p64._0, _p64._1);
			default:
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor, maybeKey, _p64._0, _p64._1, _p64._2);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor = F4(
	function (maybeKey, maybeName, isClosed, valueList) {
		var _p65 = function () {
			var _p66 = valueList;
			if (_p66.ctor === '[]') {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Nothing,
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$div,
						{ctor: '[]'},
						{ctor: '[]'})
				};
			} else {
				if (_p66._1.ctor === '[]') {
					var _p67 = _p66._0;
					switch (_p67.ctor) {
						case 'S':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Primitive':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Sequence':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(_p67._2))
							};
						case 'Dictionary':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(_p67._1))
							};
						case 'Record':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(_p67._1))
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(_p67._2))
							};
					}
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(isClosed),
						_1: isClosed ? A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{ctor: '[]'},
							{ctor: '[]'}) : _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(valueList)
					};
				}
			}
		}();
		var maybeIsClosed = _p65._0;
		var openHtml = _p65._1;
		var tinyArgs = A2(
			_elm_lang$core$List$map,
			function (_p68) {
				return _elm_lang$core$Tuple$second(
					_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p68));
			},
			valueList);
		var description = function () {
			var _p69 = {ctor: '_Tuple2', _0: maybeName, _1: tinyArgs};
			if (_p69._0.ctor === 'Nothing') {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('()'),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('( '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' )'),
									_1: {ctor: '[]'}
								},
								_p69._1._1)
						}
					};
				}
			} else {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p69._0._0),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(_elm_lang$core$Basics_ops['++'], _p69._0._0, ' ')),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{ctor: '[]'},
								_p69._1._1)
						}
					};
				}
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter, maybeKey, maybeIsClosed, description)),
				_1: {
					ctor: '::',
					_0: openHtml,
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen = function (valueList) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, valueList));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen = function (keyValuePairs) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry, keyValuePairs));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry = F2(
	function (index, _p70) {
		var _p71 = _p70;
		var _p74 = _p71._1;
		var _p73 = _p71._0;
		var _p72 = _p73;
		switch (_p72.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			default:
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$map,
							A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Key, index),
							A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$view,
								_elm_lang$core$Maybe$Just('key'),
								_p73)),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$map,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
								A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$view,
									_elm_lang$core$Maybe$Just('value'),
									_p74)),
							_1: {ctor: '[]'}
						}
					});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen = function (record) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry,
			_elm_lang$core$Dict$toList(record)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry = function (_p75) {
	var _p76 = _p75;
	var _p77 = _p76._0;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$map,
		_elm_lang$virtual_dom$VirtualDom_Expando$Field(_p77),
		A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$view,
			_elm_lang$core$Maybe$Just(_p77),
			_p76._1));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen = function (values) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, values));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary = F3(
	function (maybeKey, isClosed, keyValuePairs) {
		var starter = A2(
			_elm_lang$core$Basics_ops['++'],
			'Dict(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$List$length(keyValuePairs)),
				')'));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(keyValuePairs),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecord = F3(
	function (maybeKey, isClosed, record) {
		var _p78 = isClosed ? {
			ctor: '_Tuple3',
			_0: _elm_lang$core$Tuple$second(
				_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(record)),
			_1: _elm_lang$virtual_dom$VirtualDom_Helpers$text(''),
			_2: _elm_lang$virtual_dom$VirtualDom_Helpers$text('')
		} : {
			ctor: '_Tuple3',
			_0: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{'),
				_1: {ctor: '[]'}
			},
			_1: _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(record),
			_2: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(
						_elm_lang$core$Maybe$Just(
							{ctor: '_Tuple0'})),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				})
		};
		var start = _p78._0;
		var middle = _p78._1;
		var end = _p78._2;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						start)),
				_1: {
					ctor: '::',
					_0: middle,
					_1: {
						ctor: '::',
						_0: end,
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequence = F4(
	function (maybeKey, seqType, isClosed, valueList) {
		var starter = A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
			_elm_lang$core$List$length(valueList),
			seqType);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(valueList),
					_1: {ctor: '[]'}
				}
			});
	});

var _elm_lang$virtual_dom$VirtualDom_Report$some = function (list) {
	return !_elm_lang$core$List$isEmpty(list);
};
var _elm_lang$virtual_dom$VirtualDom_Report$TagChanges = F4(
	function (a, b, c, d) {
		return {removed: a, changed: b, added: c, argsMatch: d};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges = function (argsMatch) {
	return A4(
		_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		argsMatch);
};
var _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges = function (tagChanges) {
	return _elm_lang$core$Native_Utils.eq(
		tagChanges,
		A4(
			_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			true));
};
var _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged = function (a) {
	return {ctor: 'SomethingChanged', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$MessageChanged = F2(
	function (a, b) {
		return {ctor: 'MessageChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$VersionChanged = F2(
	function (a, b) {
		return {ctor: 'VersionChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory = {ctor: 'CorruptHistory'};
var _elm_lang$virtual_dom$VirtualDom_Report$UnionChange = F2(
	function (a, b) {
		return {ctor: 'UnionChange', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$AliasChange = function (a) {
	return {ctor: 'AliasChange', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$Fine = {ctor: 'Fine'};
var _elm_lang$virtual_dom$VirtualDom_Report$Risky = {ctor: 'Risky'};
var _elm_lang$virtual_dom$VirtualDom_Report$Impossible = {ctor: 'Impossible'};
var _elm_lang$virtual_dom$VirtualDom_Report$worstCase = F2(
	function (status, statusList) {
		worstCase:
		while (true) {
			var _p0 = statusList;
			if (_p0.ctor === '[]') {
				return status;
			} else {
				switch (_p0._0.ctor) {
					case 'Impossible':
						return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
					case 'Risky':
						var _v1 = _elm_lang$virtual_dom$VirtualDom_Report$Risky,
							_v2 = _p0._1;
						status = _v1;
						statusList = _v2;
						continue worstCase;
					default:
						var _v3 = status,
							_v4 = _p0._1;
						status = _v3;
						statusList = _v4;
						continue worstCase;
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange = function (change) {
	var _p1 = change;
	if (_p1.ctor === 'AliasChange') {
		return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
	} else {
		return ((!_p1._1.argsMatch) || (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.changed) || _elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.removed))) ? _elm_lang$virtual_dom$VirtualDom_Report$Impossible : (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.added) ? _elm_lang$virtual_dom$VirtualDom_Report$Risky : _elm_lang$virtual_dom$VirtualDom_Report$Fine);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Report$evaluate = function (report) {
	var _p2 = report;
	switch (_p2.ctor) {
		case 'CorruptHistory':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'VersionChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'MessageChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		default:
			return A2(
				_elm_lang$virtual_dom$VirtualDom_Report$worstCase,
				_elm_lang$virtual_dom$VirtualDom_Report$Fine,
				A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange, _p2._0));
	}
};

var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict = F2(
	function (f, dict) {
		return _elm_lang$core$Json_Encode$object(
			_elm_lang$core$Dict$toList(
				A2(
					_elm_lang$core$Dict$map,
					F2(
						function (key, value) {
							return f(value);
						}),
					dict)));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p1.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'tags',
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict,
						function (_p2) {
							return _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p2));
						},
						_p1.tags)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias = function (_p3) {
	var _p4 = _p3;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p4.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(_p4.tipe)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes = function (_p5) {
	var _p6 = _p5;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'message',
				_1: _elm_lang$core$Json_Encode$string(_p6.message)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aliases',
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias, _p6.aliases)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'unions',
						_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion, _p6.unions)
					},
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions = function (_p7) {
	var _p8 = _p7;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'elm',
				_1: _elm_lang$core$Json_Encode$string(_p8.elm)
			},
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encode = function (_p9) {
	var _p10 = _p9;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'versions',
				_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions(_p10.versions)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'types',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes(_p10.types)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTag = F4(
	function (tag, old, $new, changes) {
		return _elm_lang$core$Native_Utils.eq(old, $new) ? changes : _elm_lang$core$Native_Utils.update(
			changes,
			{
				changed: {ctor: '::', _0: tag, _1: changes.changed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$addTag = F3(
	function (tag, _p11, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				added: {ctor: '::', _0: tag, _1: changes.added}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$removeTag = F3(
	function (tag, _p12, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				removed: {ctor: '::', _0: tag, _1: changes.removed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion = F4(
	function (name, old, $new, changes) {
		var tagChanges = A6(
			_elm_lang$core$Dict$merge,
			_elm_lang$virtual_dom$VirtualDom_Metadata$removeTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$checkTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$addTag,
			old.tags,
			$new.tags,
			_elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges(
				_elm_lang$core$Native_Utils.eq(old.args, $new.args)));
		return _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges(tagChanges) ? changes : {
			ctor: '::',
			_0: A2(_elm_lang$virtual_dom$VirtualDom_Report$UnionChange, name, tagChanges),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias = F4(
	function (name, old, $new, changes) {
		return (_elm_lang$core$Native_Utils.eq(old.tipe, $new.tipe) && _elm_lang$core$Native_Utils.eq(old.args, $new.args)) ? changes : {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Report$AliasChange(name),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ignore = F3(
	function (key, value, report) {
		return report;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.message, $new.message)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$MessageChanged, old.message, $new.message) : _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged(
			A6(
				_elm_lang$core$Dict$merge,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				_elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				old.unions,
				$new.unions,
				A6(
					_elm_lang$core$Dict$merge,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					_elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					old.aliases,
					$new.aliases,
					{ctor: '[]'})));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$check = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.versions.elm, $new.versions.elm)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$VersionChanged, old.versions.elm, $new.versions.elm) : A2(_elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes, old.types, $new.types);
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem = F2(
	function (tipe, _p13) {
		var _p14 = _p13;
		return A2(_elm_lang$core$String$contains, _p14._1, tipe) ? _elm_lang$core$Maybe$Just(_p14._0) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Metadata = F2(
	function (a, b) {
		return {versions: a, types: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Versions = function (a) {
	return {elm: a};
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions = A2(
	_elm_lang$core$Json_Decode$map,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Versions,
	A2(_elm_lang$core$Json_Decode$field, 'elm', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Types = F3(
	function (a, b, c) {
		return {message: a, aliases: b, unions: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Alias = F2(
	function (a, b) {
		return {args: a, tipe: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Alias,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Union = F2(
	function (a, b) {
		return {args: a, tags: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Union,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'tags',
		_elm_lang$core$Json_Decode$dict(
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string))));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes = A4(
	_elm_lang$core$Json_Decode$map3,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Types,
	A2(_elm_lang$core$Json_Decode$field, 'message', _elm_lang$core$Json_Decode$string),
	A2(
		_elm_lang$core$Json_Decode$field,
		'aliases',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'unions',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion)));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Metadata,
	A2(_elm_lang$core$Json_Decode$field, 'versions', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions),
	A2(_elm_lang$core$Json_Decode$field, 'types', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Error = F2(
	function (a, b) {
		return {message: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType = F2(
	function (a, b) {
		return {name: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom = {ctor: 'VirtualDom'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Program = {ctor: 'Program'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Request = {ctor: 'Request'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Socket = {ctor: 'Socket'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Process = {ctor: 'Process'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Task = {ctor: 'Task'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder = {ctor: 'Decoder'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Function = {ctor: 'Function'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$problemTable = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Function, _1: '->'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder, _1: 'Json.Decode.Decoder'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Task, _1: 'Task.Task'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Process, _1: 'Process.Id'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Socket, _1: 'WebSocket.LowLevel.WebSocket'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Request, _1: 'Http.Request'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Program, _1: 'Platform.Program'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Node'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Attribute'},
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems = function (tipe) {
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem(tipe),
		_elm_lang$virtual_dom$VirtualDom_Metadata$problemTable);
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases = F3(
	function (name, _p15, list) {
		var _p16 = _p15;
		var _p17 = _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems(_p16.tipe);
		if (_p17.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p17),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions = F3(
	function (name, _p18, list) {
		var _p19 = _p18;
		var _p20 = A2(
			_elm_lang$core$List$concatMap,
			_elm_lang$virtual_dom$VirtualDom_Metadata$findProblems,
			_elm_lang$core$List$concat(
				_elm_lang$core$Dict$values(_p19.tags)));
		if (_p20.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p20),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable = function (_p21) {
	var _p22 = _p21;
	var _p24 = _p22.types;
	var badAliases = A3(
		_elm_lang$core$Dict$foldl,
		_elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases,
		{ctor: '[]'},
		_p24.aliases);
	var _p23 = A3(_elm_lang$core$Dict$foldl, _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions, badAliases, _p24.unions);
	if (_p23.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			A2(_elm_lang$virtual_dom$VirtualDom_Metadata$Error, _p24.message, _p23));
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decode = function (value) {
	var _p25 = A2(_elm_lang$core$Json_Decode$decodeValue, _elm_lang$virtual_dom$VirtualDom_Metadata$decoder, value);
	if (_p25.ctor === 'Err') {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.Metadata',
			{
				start: {line: 229, column: 3},
				end: {line: 239, column: 20}
			},
			_p25)('Compiler is generating bad metadata. Report this at <https://github.com/elm-lang/virtual-dom/issues>.');
	} else {
		var _p28 = _p25._0;
		var _p27 = _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable(_p28);
		if (_p27.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(_p28);
		} else {
			return _elm_lang$core$Result$Err(_p27._0);
		}
	}
};

var _elm_lang$virtual_dom$VirtualDom_History$viewMessage = F3(
	function (currentIndex, index, msg) {
		var messageName = _elm_lang$virtual_dom$Native_Debug.messageToString(msg);
		var className = _elm_lang$core$Native_Utils.eq(currentIndex, index) ? 'messages-entry messages-entry-selected' : 'messages-entry';
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$on,
						'click',
						_elm_lang$core$Json_Decode$succeed(index)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$span,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-content'),
						_1: {
							ctor: '::',
							_0: A2(_elm_lang$virtual_dom$VirtualDom_Helpers$attribute, 'title', messageName),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(messageName),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-index'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								_elm_lang$core$Basics$toString(index)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_History$consMsg = F3(
	function (currentIndex, msg, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		return {
			ctor: '_Tuple2',
			_0: _p2 - 1,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewMessage, currentIndex, _p2, msg),
				_1: _p1._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot = F3(
	function (currentIndex, index, _p3) {
		var _p4 = _p3;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldl,
					_elm_lang$virtual_dom$VirtualDom_History$consMsg(currentIndex),
					{
						ctor: '_Tuple2',
						_0: index - 1,
						_1: {ctor: '[]'}
					},
					_p4.messages)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$undone = function (getResult) {
	var _p5 = getResult;
	if (_p5.ctor === 'Done') {
		return {ctor: '_Tuple2', _0: _p5._1, _1: _p5._0};
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.History',
			{
				start: {line: 195, column: 3},
				end: {line: 200, column: 39}
			},
			_p5)('Bug in History.get');
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$elmToJs = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$encodeHelp = F2(
	function (snapshot, allMessages) {
		return A3(
			_elm_lang$core$Array$foldl,
			F2(
				function (elm, msgs) {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_History$elmToJs(elm),
						_1: msgs
					};
				}),
			allMessages,
			snapshot.messages);
	});
var _elm_lang$virtual_dom$VirtualDom_History$encode = function (_p7) {
	var _p8 = _p7;
	var recentJson = A2(
		_elm_lang$core$List$map,
		_elm_lang$virtual_dom$VirtualDom_History$elmToJs,
		_elm_lang$core$List$reverse(_p8.recent.messages));
	return _elm_lang$core$Json_Encode$list(
		A3(_elm_lang$core$Array$foldr, _elm_lang$virtual_dom$VirtualDom_History$encodeHelp, recentJson, _p8.snapshots));
};
var _elm_lang$virtual_dom$VirtualDom_History$jsToElm = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$initialModel = function (_p9) {
	var _p10 = _p9;
	var _p11 = A2(_elm_lang$core$Array$get, 0, _p10.snapshots);
	if (_p11.ctor === 'Just') {
		return _p11._0.model;
	} else {
		return _p10.recent.model;
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$size = function (history) {
	return history.numMessages;
};
var _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize = 64;
var _elm_lang$virtual_dom$VirtualDom_History$consSnapshot = F3(
	function (currentIndex, snapshot, _p12) {
		var _p13 = _p12;
		var _p14 = _p13._0;
		var nextIndex = _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize;
		var currentIndexHelp = ((_elm_lang$core$Native_Utils.cmp(nextIndex, currentIndex) < 1) && (_elm_lang$core$Native_Utils.cmp(currentIndex, _p14) < 0)) ? currentIndex : -1;
		return {
			ctor: '_Tuple2',
			_0: _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot, currentIndexHelp, _p14, snapshot),
				_1: _p13._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots = F2(
	function (currentIndex, snapshots) {
		var highIndex = _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize * _elm_lang$core$Array$length(snapshots);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$consSnapshot(currentIndex),
					{
						ctor: '_Tuple2',
						_0: highIndex,
						_1: {ctor: '[]'}
					},
					snapshots)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$view = F2(
	function (maybeIndex, _p15) {
		var _p16 = _p15;
		var _p17 = function () {
			var _p18 = maybeIndex;
			if (_p18.ctor === 'Nothing') {
				return {ctor: '_Tuple2', _0: -1, _1: 'debugger-sidebar-messages'};
			} else {
				return {ctor: '_Tuple2', _0: _p18._0, _1: 'debugger-sidebar-messages-paused'};
			}
		}();
		var index = _p17._0;
		var className = _p17._1;
		var oldStuff = A3(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy2, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots, index, _p16.snapshots);
		var newStuff = _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				_elm_lang$virtual_dom$VirtualDom_History$consMsg(index),
				{
					ctor: '_Tuple2',
					_0: _p16.numMessages - 1,
					_1: {ctor: '[]'}
				},
				_p16.recent.messages));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {ctor: '[]'}
			},
			{ctor: '::', _0: oldStuff, _1: newStuff});
	});
var _elm_lang$virtual_dom$VirtualDom_History$History = F3(
	function (a, b, c) {
		return {snapshots: a, recent: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$RecentHistory = F3(
	function (a, b, c) {
		return {model: a, messages: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$empty = function (model) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_History$History,
		_elm_lang$core$Array$empty,
		A3(
			_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
			model,
			{ctor: '[]'},
			0),
		0);
};
var _elm_lang$virtual_dom$VirtualDom_History$Snapshot = F2(
	function (a, b) {
		return {model: a, messages: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$addRecent = F3(
	function (msg, newModel, _p19) {
		var _p20 = _p19;
		var _p23 = _p20.numMessages;
		var _p22 = _p20.model;
		var _p21 = _p20.messages;
		return _elm_lang$core$Native_Utils.eq(_p23, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) ? {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$virtual_dom$VirtualDom_History$Snapshot,
					_p22,
					_elm_lang$core$Array$fromList(_p21))),
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				newModel,
				{
					ctor: '::',
					_0: msg,
					_1: {ctor: '[]'}
				},
				1)
		} : {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Nothing,
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				_p22,
				{ctor: '::', _0: msg, _1: _p21},
				_p23 + 1)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$add = F3(
	function (msg, model, _p24) {
		var _p25 = _p24;
		var _p28 = _p25.snapshots;
		var _p27 = _p25.numMessages;
		var _p26 = A3(_elm_lang$virtual_dom$VirtualDom_History$addRecent, msg, model, _p25.recent);
		if (_p26._0.ctor === 'Just') {
			return A3(
				_elm_lang$virtual_dom$VirtualDom_History$History,
				A2(_elm_lang$core$Array$push, _p26._0._0, _p28),
				_p26._1,
				_p27 + 1);
		} else {
			return A3(_elm_lang$virtual_dom$VirtualDom_History$History, _p28, _p26._1, _p27 + 1);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$decoder = F2(
	function (initialModel, update) {
		var addMessage = F2(
			function (rawMsg, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				var msg = _elm_lang$virtual_dom$VirtualDom_History$jsToElm(rawMsg);
				return {
					ctor: '_Tuple2',
					_0: A2(update, msg, _p31),
					_1: A3(_elm_lang$virtual_dom$VirtualDom_History$add, msg, _p31, _p30._1)
				};
			});
		var updateModel = function (rawMsgs) {
			return A3(
				_elm_lang$core$List$foldl,
				addMessage,
				{
					ctor: '_Tuple2',
					_0: initialModel,
					_1: _elm_lang$virtual_dom$VirtualDom_History$empty(initialModel)
				},
				rawMsgs);
		};
		return A2(
			_elm_lang$core$Json_Decode$map,
			updateModel,
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value));
	});
var _elm_lang$virtual_dom$VirtualDom_History$Done = F2(
	function (a, b) {
		return {ctor: 'Done', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$Stepping = F2(
	function (a, b) {
		return {ctor: 'Stepping', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$getHelp = F3(
	function (update, msg, getResult) {
		var _p32 = getResult;
		if (_p32.ctor === 'Done') {
			return getResult;
		} else {
			var _p34 = _p32._0;
			var _p33 = _p32._1;
			return _elm_lang$core$Native_Utils.eq(_p34, 0) ? A2(
				_elm_lang$virtual_dom$VirtualDom_History$Done,
				msg,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33))) : A2(
				_elm_lang$virtual_dom$VirtualDom_History$Stepping,
				_p34 - 1,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33)));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$get = F3(
	function (update, index, _p35) {
		var _p36 = _p35;
		var _p39 = _p36.recent;
		var snapshotMax = _p36.numMessages - _p39.numMessages;
		if (_elm_lang$core$Native_Utils.cmp(index, snapshotMax) > -1) {
			return _elm_lang$virtual_dom$VirtualDom_History$undone(
				A3(
					_elm_lang$core$List$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
					A2(_elm_lang$virtual_dom$VirtualDom_History$Stepping, index - snapshotMax, _p39.model),
					_p39.messages));
		} else {
			var _p37 = A2(_elm_lang$core$Array$get, (index / _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) | 0, _p36.snapshots);
			if (_p37.ctor === 'Nothing') {
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.History',
					{
						start: {line: 165, column: 7},
						end: {line: 171, column: 95}
					},
					_p37)('UI should only let you ask for real indexes!');
			} else {
				return _elm_lang$virtual_dom$VirtualDom_History$undone(
					A3(
						_elm_lang$core$Array$foldr,
						_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
						A2(
							_elm_lang$virtual_dom$VirtualDom_History$Stepping,
							A2(_elm_lang$core$Basics$rem, index, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize),
							_p37._0.model),
						_p37._0.messages));
			}
		}
	});

var _elm_lang$virtual_dom$VirtualDom_Overlay$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\n.elm-overlay {\n  position: fixed;\n  top: 0;\n  left: 0;\n  width: 100%;\n  height: 100%;\n  color: white;\n  pointer-events: none;\n  font-family: \'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif;\n}\n\n.elm-overlay-resume {\n  width: 100%;\n  height: 100%;\n  cursor: pointer;\n  text-align: center;\n  pointer-events: auto;\n  background-color: rgba(200, 200, 200, 0.7);\n}\n\n.elm-overlay-resume-words {\n  position: absolute;\n  top: calc(50% - 40px);\n  font-size: 80px;\n  line-height: 80px;\n  height: 80px;\n  width: 100%;\n}\n\n.elm-mini-controls {\n  position: fixed;\n  bottom: 0;\n  right: 6px;\n  border-radius: 4px;\n  background-color: rgb(61, 61, 61);\n  font-family: monospace;\n  pointer-events: auto;\n}\n\n.elm-mini-controls-button {\n  padding: 6px;\n  cursor: pointer;\n  text-align: center;\n  min-width: 24ch;\n}\n\n.elm-mini-controls-import-export {\n  padding: 4px 0;\n  font-size: 0.8em;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message {\n  position: absolute;\n  width: 600px;\n  height: 100%;\n  padding-left: calc(50% - 300px);\n  padding-right: calc(50% - 300px);\n  background-color: rgba(200, 200, 200, 0.7);\n  pointer-events: auto;\n}\n\n.elm-overlay-message-title {\n  font-size: 36px;\n  height: 80px;\n  background-color: rgb(50, 50, 50);\n  padding-left: 22px;\n  vertical-align: middle;\n  line-height: 80px;\n}\n\n.elm-overlay-message-details {\n  padding: 8px 20px;\n  overflow-y: auto;\n  max-height: calc(100% - 156px);\n  background-color: rgb(61, 61, 61);\n}\n\n.elm-overlay-message-details-type {\n  font-size: 1.5em;\n}\n\n.elm-overlay-message-details ul {\n  list-style-type: none;\n  padding-left: 20px;\n}\n\n.elm-overlay-message-details ul ul {\n  list-style-type: disc;\n  padding-left: 2em;\n}\n\n.elm-overlay-message-details li {\n  margin: 8px 0;\n}\n\n.elm-overlay-message-buttons {\n  height: 60px;\n  line-height: 60px;\n  text-align: right;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message-buttons button {\n  margin-right: 20px;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport = F3(
	function (props, importMsg, exportMsg) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			props,
			{
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, importMsg, 'Import'),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, exportMsg, 'Export'),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls = F2(
	function (config, numMsgs) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.open),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-button'),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'Explore History (',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(numMsgs),
									')'))),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-import-export'),
							_1: {ctor: '[]'}
						},
						config.importHistory,
						config.exportHistory),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$addCommas = function (items) {
	var _p0 = items;
	if (_p0.ctor === '[]') {
		return '';
	} else {
		if (_p0._1.ctor === '[]') {
			return _p0._0;
		} else {
			if (_p0._1._1.ctor === '[]') {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p0._0,
					A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._1._0));
			} else {
				return A2(
					_elm_lang$core$String$join,
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p0._1,
						{
							ctor: '::',
							_0: A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._0),
							_1: {ctor: '[]'}
						}));
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString = function (problem) {
	var _p1 = problem;
	switch (_p1.ctor) {
		case 'Function':
			return 'functions';
		case 'Decoder':
			return 'JSON decoders';
		case 'Task':
			return 'tasks';
		case 'Process':
			return 'processes';
		case 'Socket':
			return 'web sockets';
		case 'Request':
			return 'HTTP requests';
		case 'Program':
			return 'programs';
		default:
			return 'virtual DOM values';
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2 = '\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1 = '\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode = function (name) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'code',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(name),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMention = F2(
	function (tags, verbed) {
		var _p2 = A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Overlay$viewCode,
			_elm_lang$core$List$reverse(tags));
		if (_p2.ctor === '[]') {
			return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
		} else {
			if (_p2._1.ctor === '[]') {
				return A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'li',
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
						_1: {
							ctor: '::',
							_0: _p2._0,
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
								_1: {ctor: '[]'}
							}
						}
					});
			} else {
				if (_p2._1._1.ctor === '[]') {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: {
								ctor: '::',
								_0: _p2._1._0,
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						});
				} else {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								A2(
									_elm_lang$core$List$intersperse,
									_elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
									_elm_lang$core$List$reverse(_p2._1)),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								})
						});
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange = function (change) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		function () {
			var _p3 = change;
			if (_p3.ctor === 'AliasChange') {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.removed, 'Removed '),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.changed, 'Changed '),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.added, 'Added '),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _p3._1.argsMatch ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Helpers$text('This may be due to the fact that the type variable names changed.'),
							_1: {ctor: '[]'}
						}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType = function (_p4) {
	var _p5 = _p4;
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p5.name),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
					A2(
						_elm_lang$core$Basics_ops['++'],
						' can contain ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$virtual_dom$VirtualDom_Overlay$addCommas(
								A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString, _p5.problems)),
							'.'))),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata = function (_p6) {
	var _p7 = _p6;
	return {
		ctor: '::',
		_0: A3(
			_elm_lang$virtual_dom$VirtualDom_Helpers$node,
			'p',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('The '),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p7.message),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' type of your program cannot be reliably serialized for history files.'),
						_1: {ctor: '[]'}
					}
				}
			}),
		_1: {
			ctor: '::',
			_0: A3(
				_elm_lang$virtual_dom$VirtualDom_Helpers$node,
				'p',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'ul',
					{ctor: '[]'},
					A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType, _p7.problems)),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$a,
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$href('https://guide.elm-lang.org/types/union_types.html'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('union types'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', in your messages. From there, your '),
									_1: {
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode('update'),
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky = '\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad = '\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewReport = F2(
	function (isBad, report) {
		var _p8 = report;
		switch (_p8.ctor) {
			case 'CorruptHistory':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Looks like this history file is corrupt. I cannot understand it.'),
					_1: {ctor: '[]'}
				};
			case 'VersionChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'This history was created with Elm ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_p8._0,
								A2(
									_elm_lang$core$Basics_ops['++'],
									', but you are using Elm ',
									A2(_elm_lang$core$Basics_ops['++'], _p8._1, ' right now.'))))),
					_1: {ctor: '[]'}
				};
			case 'MessageChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(_elm_lang$core$Basics_ops['++'], 'To import some other history, the overall message type must', ' be the same. The old history has ')),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages, but the new program works with '),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._1),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages.'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				};
			default:
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								isBad ? _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad : _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange, _p8._0)),
						_1: {ctor: '[]'}
					}
				};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume = function (config) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume'),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.resume),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume-words'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Click to Resume'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(_elm_lang$core$Json_Decode$field, 'metadata', _elm_lang$virtual_dom$VirtualDom_Metadata$decoder),
	A2(_elm_lang$core$Json_Decode$field, 'history', _elm_lang$core$Json_Decode$value));
var _elm_lang$virtual_dom$VirtualDom_Overlay$close = F2(
	function (msg, state) {
		var _p9 = state;
		switch (_p9.ctor) {
			case 'None':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadMetadata':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadImport':
				return _elm_lang$core$Maybe$Nothing;
			default:
				var _p10 = msg;
				if (_p10.ctor === 'Cancel') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					return _elm_lang$core$Maybe$Just(_p9._1);
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$isBlocking = function (state) {
	var _p11 = state;
	if (_p11.ctor === 'None') {
		return false;
	} else {
		return true;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Config = F5(
	function (a, b, c, d, e) {
		return {resume: a, open: b, importHistory: c, exportHistory: d, wrap: e};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport = F2(
	function (a, b) {
		return {ctor: 'RiskyImport', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport = function (a) {
	return {ctor: 'BadImport', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport = _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(_elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory);
var _elm_lang$virtual_dom$VirtualDom_Overlay$assessImport = F2(
	function (metadata, jsonString) {
		var _p12 = A2(_elm_lang$core$Json_Decode$decodeString, _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder, jsonString);
		if (_p12.ctor === 'Err') {
			return _elm_lang$core$Result$Err(_elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport);
		} else {
			var _p14 = _p12._0._1;
			var report = A2(_elm_lang$virtual_dom$VirtualDom_Metadata$check, _p12._0._0, metadata);
			var _p13 = _elm_lang$virtual_dom$VirtualDom_Report$evaluate(report);
			switch (_p13.ctor) {
				case 'Impossible':
					return _elm_lang$core$Result$Err(
						_elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(report));
				case 'Risky':
					return _elm_lang$core$Result$Err(
						A2(_elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport, report, _p14));
				default:
					return _elm_lang$core$Result$Ok(_p14);
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata = function (a) {
	return {ctor: 'BadMetadata', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata = _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata;
var _elm_lang$virtual_dom$VirtualDom_Overlay$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$none = _elm_lang$virtual_dom$VirtualDom_Overlay$None;
var _elm_lang$virtual_dom$VirtualDom_Overlay$Proceed = {ctor: 'Proceed'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Cancel = {ctor: 'Cancel'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons = function (buttons) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-buttons'),
			_1: {ctor: '[]'}
		},
		function () {
			var _p15 = buttons;
			if (_p15.ctor === 'Accept') {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Cancel),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'button',
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._1),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Message = {ctor: 'Message'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage = F4(
	function (config, title, details, buttons) {
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$Message,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-title'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(title),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$div,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details'),
									_1: {ctor: '[]'}
								},
								details),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									config.wrap,
									_elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons(buttons)),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Pause = {ctor: 'Pause'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Normal = {ctor: 'Normal'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Choose = F2(
	function (a, b) {
		return {ctor: 'Choose', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Accept = function (a) {
	return {ctor: 'Accept', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p16 = state;
		switch (_p16.ctor) {
			case 'None':
				var miniControls = isOpen ? {ctor: '[]'} : {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls, config, numMsgs),
					_1: {ctor: '[]'}
				};
				return {
					ctor: '_Tuple2',
					_0: isPaused ? _elm_lang$virtual_dom$VirtualDom_Overlay$Pause : _elm_lang$virtual_dom$VirtualDom_Overlay$Normal,
					_1: (isPaused && (!isOpen)) ? {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume(config),
						_1: miniControls
					} : miniControls
				};
			case 'BadMetadata':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot use Import or Export',
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata(_p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			case 'BadImport':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot Import History',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, true, _p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			default:
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Warning',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, false, _p16._0),
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$Choose, 'Cancel', 'Import Anyway'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$view = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p17 = A5(_elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp, config, isPaused, isOpen, numMsgs, state);
		var block = _p17._0;
		var nodes = _p17._1;
		return {
			ctor: '_Tuple2',
			_0: block,
			_1: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay'),
					_1: {ctor: '[]'}
				},
				{ctor: '::', _0: _elm_lang$virtual_dom$VirtualDom_Overlay$styles, _1: nodes})
		};
	});

var _elm_lang$virtual_dom$VirtualDom_Debug$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\nhtml {\n    overflow: hidden;\n    height: 100%;\n}\n\nbody {\n    height: 100%;\n    overflow: auto;\n}\n\n#debugger {\n  width: 100%\n  height: 100%;\n  font-family: monospace;\n}\n\n#values {\n  display: block;\n  float: left;\n  height: 100%;\n  width: calc(100% - 30ch);\n  margin: 0;\n  overflow: auto;\n  cursor: default;\n}\n\n.debugger-sidebar {\n  display: block;\n  float: left;\n  width: 30ch;\n  height: 100%;\n  color: white;\n  background-color: rgb(61, 61, 61);\n}\n\n.debugger-sidebar-controls {\n  width: 100%;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.debugger-sidebar-controls-import-export {\n  width: 100%;\n  height: 24px;\n  line-height: 24px;\n  font-size: 12px;\n}\n\n.debugger-sidebar-controls-resume {\n  width: 100%;\n  height: 30px;\n  line-height: 30px;\n  cursor: pointer;\n}\n\n.debugger-sidebar-controls-resume:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.debugger-sidebar-messages {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 24px);\n}\n\n.debugger-sidebar-messages-paused {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 54px);\n}\n\n.messages-entry {\n  cursor: pointer;\n  width: 100%;\n}\n\n.messages-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.messages-entry-selected, .messages-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.messages-entry-content {\n  width: calc(100% - 7ch);\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-left: 1ch;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.messages-entry-index {\n  color: #666;\n  width: 5ch;\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-right: 1ch;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel = function (state) {
	var _p0 = state;
	if (_p0.ctor === 'Running') {
		return _p0._0;
	} else {
		return _p0._2;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata = F2(
	function (model, func) {
		var _p1 = model.metadata;
		if (_p1.ctor === 'Ok') {
			return func(_p1._0);
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata(_p1._0)
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Model = F6(
	function (a, b, c, d, e, f) {
		return {history: a, state: b, expando: c, metadata: d, overlay: e, isDebuggerOpen: f};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Paused = F3(
	function (a, b, c) {
		return {ctor: 'Paused', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Running = function (a) {
	return {ctor: 'Running', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory = F3(
	function (rawHistory, userUpdate, model) {
		var pureUserUpdate = F2(
			function (msg, userModel) {
				return _elm_lang$core$Tuple$first(
					A2(userUpdate, msg, userModel));
			});
		var initialUserModel = _elm_lang$virtual_dom$VirtualDom_History$initialModel(model.history);
		var decoder = A2(_elm_lang$virtual_dom$VirtualDom_History$decoder, initialUserModel, pureUserUpdate);
		var _p2 = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, rawHistory);
		if (_p2.ctor === 'Err') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport}),
				{ctor: '[]'});
		} else {
			var _p3 = _p2._0._0;
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						history: _p2._0._1,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p3),
						expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p3),
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg = function (a) {
	return {ctor: 'OverlayMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Upload = function (a) {
	return {ctor: 'Upload', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$upload = A2(_elm_lang$core$Task$perform, _elm_lang$virtual_dom$VirtualDom_Debug$Upload, _elm_lang$virtual_dom$Native_Debug.upload);
var _elm_lang$virtual_dom$VirtualDom_Debug$Export = {ctor: 'Export'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Import = {ctor: 'Import'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Down = {ctor: 'Down'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Up = {ctor: 'Up'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Close = {ctor: 'Close'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Open = {ctor: 'Open'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Jump = function (a) {
	return {ctor: 'Jump', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Resume = {ctor: 'Resume'};
var _elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig = {resume: _elm_lang$virtual_dom$VirtualDom_Debug$Resume, open: _elm_lang$virtual_dom$VirtualDom_Debug$Open, importHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Import, exportHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Export, wrap: _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewIn = function (_p4) {
	var _p5 = _p4;
	var isPaused = function () {
		var _p6 = _p5.state;
		if (_p6.ctor === 'Running') {
			return false;
		} else {
			return true;
		}
	}();
	return A5(
		_elm_lang$virtual_dom$VirtualDom_Overlay$view,
		_elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig,
		isPaused,
		_p5.isDebuggerOpen,
		_elm_lang$virtual_dom$VirtualDom_History$size(_p5.history),
		_p5.overlay);
};
var _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton = A2(
	_elm_lang$virtual_dom$VirtualDom_Helpers$div,
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Debug$Resume),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-resume'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Resume'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton = function (maybeIndex) {
	var _p7 = maybeIndex;
	if (_p7.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$playButton = function (maybeIndex) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton(maybeIndex),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-import-export'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Import, 'Import'),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Export, 'Export'),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar = F2(
	function (state, history) {
		var maybeIndex = function () {
			var _p8 = state;
			if (_p8.ctor === 'Running') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(_p8._0);
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					_elm_lang$virtual_dom$VirtualDom_Debug$Jump,
					A2(_elm_lang$virtual_dom$VirtualDom_History$view, maybeIndex, history)),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Debug$playButton(maybeIndex),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg = function (a) {
	return {ctor: 'ExpandoMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewOut = function (_p9) {
	var _p10 = _p9;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('debugger'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$styles,
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar, _p10.state, _p10.history),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$map,
						_elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg,
						A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('values'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$view, _elm_lang$core$Maybe$Nothing, _p10.expando),
								_1: {ctor: '[]'}
							})),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg = function (a) {
	return {ctor: 'UserMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapInit = F2(
	function (metadata, _p11) {
		var _p12 = _p11;
		var _p13 = _p12._0;
		return A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			{
				history: _elm_lang$virtual_dom$VirtualDom_History$empty(_p13),
				state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p13),
				expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p13),
				metadata: _elm_lang$virtual_dom$VirtualDom_Metadata$decode(metadata),
				overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none,
				isDebuggerOpen: false
			},
			{
				ctor: '::',
				_0: A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, _p12._1),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs = F2(
	function (userSubscriptions, _p14) {
		var _p15 = _p14;
		return A2(
			_elm_lang$core$Platform_Sub$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userSubscriptions(
				_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p15.state)));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapView = F2(
	function (userView, _p16) {
		var _p17 = _p16;
		var currentModel = function () {
			var _p18 = _p17.state;
			if (_p18.ctor === 'Running') {
				return _p18._0;
			} else {
				return _p18._1;
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userView(currentModel));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$NoOp = {ctor: 'NoOp'};
var _elm_lang$virtual_dom$VirtualDom_Debug$download = F2(
	function (metadata, history) {
		var json = _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'metadata',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encode(metadata)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'history',
						_1: _elm_lang$virtual_dom$VirtualDom_History$encode(history)
					},
					_1: {ctor: '[]'}
				}
			});
		var historyLength = _elm_lang$virtual_dom$VirtualDom_History$size(history);
		return A2(
			_elm_lang$core$Task$perform,
			function (_p19) {
				return _elm_lang$virtual_dom$VirtualDom_Debug$NoOp;
			},
			A2(_elm_lang$virtual_dom$Native_Debug.download, historyLength, json));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$runIf = F2(
	function (bool, task) {
		return bool ? A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(_elm_lang$virtual_dom$VirtualDom_Debug$NoOp),
			task) : _elm_lang$core$Platform_Cmd$none;
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg = F4(
	function (userUpdate, scrollTask, userMsg, _p20) {
		var _p21 = _p20;
		var _p25 = _p21.state;
		var _p24 = _p21;
		var userModel = _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p25);
		var newHistory = A3(_elm_lang$virtual_dom$VirtualDom_History$add, userMsg, userModel, _p21.history);
		var _p22 = A2(userUpdate, userMsg, userModel);
		var newUserModel = _p22._0;
		var userCmds = _p22._1;
		var commands = A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, userCmds);
		var _p23 = _p25;
		if (_p23.ctor === 'Running') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(newUserModel),
						expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, newUserModel, _p21.expando)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, _p24.isDebuggerOpen, scrollTask),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: A3(_elm_lang$virtual_dom$VirtualDom_Debug$Paused, _p23._0, _p23._1, newUserModel)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {ctor: '[]'}
				});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate = F4(
	function (userUpdate, scrollTask, msg, model) {
		wrapUpdate:
		while (true) {
			var _p26 = msg;
			switch (_p26.ctor) {
				case 'NoOp':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{ctor: '[]'});
				case 'UserMsg':
					return A4(_elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg, userUpdate, scrollTask, _p26._0, model);
				case 'ExpandoMsg':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p26._0, model.expando)
							}),
						{ctor: '[]'});
				case 'Resume':
					var _p27 = model.state;
					if (_p27.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p28 = _p27._2;
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{
									state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p28),
									expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, _p28, model.expando)
								}),
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, model.isDebuggerOpen, scrollTask),
								_1: {ctor: '[]'}
							});
					}
				case 'Jump':
					var _p30 = _p26._0;
					var _p29 = A3(_elm_lang$virtual_dom$VirtualDom_History$get, userUpdate, _p30, model.history);
					var indexModel = _p29._0;
					var indexMsg = _p29._1;
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								state: A3(
									_elm_lang$virtual_dom$VirtualDom_Debug$Paused,
									_p30,
									indexModel,
									_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(model.state)),
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, indexModel, model.expando)
							}),
						{ctor: '[]'});
				case 'Open':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: true}),
						{ctor: '[]'});
				case 'Close':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: false}),
						{ctor: '[]'});
				case 'Up':
					var index = function () {
						var _p31 = model.state;
						if (_p31.ctor === 'Paused') {
							return _p31._0;
						} else {
							return _elm_lang$virtual_dom$VirtualDom_History$size(model.history);
						}
					}();
					if (_elm_lang$core$Native_Utils.cmp(index, 0) > 0) {
						var _v17 = userUpdate,
							_v18 = scrollTask,
							_v19 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(index - 1),
							_v20 = model;
						userUpdate = _v17;
						scrollTask = _v18;
						msg = _v19;
						model = _v20;
						continue wrapUpdate;
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					}
				case 'Down':
					var _p32 = model.state;
					if (_p32.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p33 = _p32._0;
						if (_elm_lang$core$Native_Utils.eq(
							_p33,
							_elm_lang$virtual_dom$VirtualDom_History$size(model.history) - 1)) {
							var _v22 = userUpdate,
								_v23 = scrollTask,
								_v24 = _elm_lang$virtual_dom$VirtualDom_Debug$Resume,
								_v25 = model;
							userUpdate = _v22;
							scrollTask = _v23;
							msg = _v24;
							model = _v25;
							continue wrapUpdate;
						} else {
							var _v26 = userUpdate,
								_v27 = scrollTask,
								_v28 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(_p33 + 1),
								_v29 = model;
							userUpdate = _v26;
							scrollTask = _v27;
							msg = _v28;
							model = _v29;
							continue wrapUpdate;
						}
					}
				case 'Import':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (_p34) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Debug$upload,
									_1: {ctor: '[]'}
								});
						});
				case 'Export':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$download, metadata, model.history),
									_1: {ctor: '[]'}
								});
						});
				case 'Upload':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							var _p35 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$assessImport, metadata, _p26._0);
							if (_p35.ctor === 'Err') {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									_elm_lang$core$Native_Utils.update(
										model,
										{overlay: _p35._0}),
									{ctor: '[]'});
							} else {
								return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p35._0, userUpdate, model);
							}
						});
				default:
					var _p36 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$close, _p26._0, model.overlay);
					if (_p36.ctor === 'Nothing') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none}),
							{ctor: '[]'});
					} else {
						return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p36._0, userUpdate, model);
					}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrap = F2(
	function (metadata, _p37) {
		var _p38 = _p37;
		return {
			init: A2(_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit, metadata, _p38.init),
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p38.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p38.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p38.subscriptions)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags = F2(
	function (metadata, _p39) {
		var _p40 = _p39;
		return {
			init: function (flags) {
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit,
					metadata,
					_p40.init(flags));
			},
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p40.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p40.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p40.subscriptions)
		};
	});

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					},
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$core$Platform$sendToSelf(router),
									_elm_lang$window$Window$size);
							})));
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _mdgriffith$elm_style_animation$Animation_Model$matchPoints = F2(
	function (points1, points2) {
		var diff = _elm_lang$core$List$length(points1) - _elm_lang$core$List$length(points2);
		if (_elm_lang$core$Native_Utils.cmp(diff, 0) > 0) {
			var _p0 = _elm_lang$core$List$head(
				_elm_lang$core$List$reverse(points2));
			if (_p0.ctor === 'Nothing') {
				return {ctor: '_Tuple2', _0: points1, _1: points2};
			} else {
				return {
					ctor: '_Tuple2',
					_0: points1,
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						points2,
						A2(
							_elm_lang$core$List$repeat,
							_elm_lang$core$Basics$abs(diff),
							_p0._0))
				};
			}
		} else {
			if (_elm_lang$core$Native_Utils.cmp(diff, 0) < 0) {
				var _p1 = _elm_lang$core$List$head(
					_elm_lang$core$List$reverse(points1));
				if (_p1.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: points1, _1: points2};
				} else {
					return {
						ctor: '_Tuple2',
						_0: A2(
							_elm_lang$core$Basics_ops['++'],
							points1,
							A2(
								_elm_lang$core$List$repeat,
								_elm_lang$core$Basics$abs(diff),
								_p1._0)),
						_1: points2
					};
				}
			} else {
				return {ctor: '_Tuple2', _0: points1, _1: points2};
			}
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$vTolerance = 0.1;
var _mdgriffith$elm_style_animation$Animation_Model$tolerance = 1.0e-2;
var _mdgriffith$elm_style_animation$Animation_Model$isCmdDone = function (cmd) {
	var motionDone = function (motion) {
		return _elm_lang$core$Native_Utils.eq(motion.velocity, 0) && _elm_lang$core$Native_Utils.eq(motion.position, motion.target);
	};
	var _p2 = cmd;
	switch (_p2.ctor) {
		case 'Move':
			return motionDone(_p2._0) && motionDone(_p2._1);
		case 'MoveTo':
			return motionDone(_p2._0) && motionDone(_p2._1);
		case 'Line':
			return motionDone(_p2._0) && motionDone(_p2._1);
		case 'LineTo':
			return motionDone(_p2._0) && motionDone(_p2._1);
		case 'Horizontal':
			return motionDone(_p2._0);
		case 'HorizontalTo':
			return motionDone(_p2._0);
		case 'Vertical':
			return motionDone(_p2._0);
		case 'VerticalTo':
			return motionDone(_p2._0);
		case 'Curve':
			var _p5 = _p2._0.point;
			var _p4 = _p2._0.control2;
			var _p3 = _p2._0.control1;
			return motionDone(
				_elm_lang$core$Tuple$first(_p3)) && (motionDone(
				_elm_lang$core$Tuple$second(_p3)) && (motionDone(
				_elm_lang$core$Tuple$first(_p4)) && (motionDone(
				_elm_lang$core$Tuple$second(_p4)) && (motionDone(
				_elm_lang$core$Tuple$first(_p5)) && motionDone(
				_elm_lang$core$Tuple$second(_p5))))));
		case 'CurveTo':
			var _p8 = _p2._0.point;
			var _p7 = _p2._0.control2;
			var _p6 = _p2._0.control1;
			return motionDone(
				_elm_lang$core$Tuple$first(_p6)) && (motionDone(
				_elm_lang$core$Tuple$second(_p6)) && (motionDone(
				_elm_lang$core$Tuple$first(_p7)) && (motionDone(
				_elm_lang$core$Tuple$second(_p7)) && (motionDone(
				_elm_lang$core$Tuple$first(_p8)) && motionDone(
				_elm_lang$core$Tuple$second(_p8))))));
		case 'Quadratic':
			var _p10 = _p2._0.point;
			var _p9 = _p2._0.control;
			return motionDone(
				_elm_lang$core$Tuple$first(_p9)) && (motionDone(
				_elm_lang$core$Tuple$second(_p9)) && (motionDone(
				_elm_lang$core$Tuple$first(_p10)) && motionDone(
				_elm_lang$core$Tuple$second(_p10))));
		case 'QuadraticTo':
			var _p12 = _p2._0.point;
			var _p11 = _p2._0.control;
			return motionDone(
				_elm_lang$core$Tuple$first(_p11)) && (motionDone(
				_elm_lang$core$Tuple$second(_p11)) && (motionDone(
				_elm_lang$core$Tuple$first(_p12)) && motionDone(
				_elm_lang$core$Tuple$second(_p12))));
		case 'SmoothQuadratic':
			return A2(
				_elm_lang$core$List$all,
				function (_p13) {
					var _p14 = _p13;
					return motionDone(_p14._0) && motionDone(_p14._1);
				},
				_p2._0);
		case 'SmoothQuadraticTo':
			return A2(
				_elm_lang$core$List$all,
				function (_p15) {
					var _p16 = _p15;
					return motionDone(_p16._0) && motionDone(_p16._1);
				},
				_p2._0);
		case 'Smooth':
			return A2(
				_elm_lang$core$List$all,
				function (_p17) {
					var _p18 = _p17;
					return motionDone(_p18._0) && motionDone(_p18._1);
				},
				_p2._0);
		case 'SmoothTo':
			return A2(
				_elm_lang$core$List$all,
				function (_p19) {
					var _p20 = _p19;
					return motionDone(_p20._0) && motionDone(_p20._1);
				},
				_p2._0);
		case 'ClockwiseArc':
			var _p21 = _p2._0;
			return motionDone(_p21.x) && (motionDone(_p21.y) && (motionDone(_p21.radius) && (motionDone(_p21.startAngle) && motionDone(_p21.endAngle))));
		case 'AntiClockwiseArc':
			var _p22 = _p2._0;
			return motionDone(_p22.x) && (motionDone(_p22.y) && (motionDone(_p22.radius) && (motionDone(_p22.startAngle) && motionDone(_p22.endAngle))));
		default:
			return true;
	}
};
var _mdgriffith$elm_style_animation$Animation_Model$isDone = function (property) {
	var motionDone = function (motion) {
		var _p23 = motion.interpolation;
		switch (_p23.ctor) {
			case 'Spring':
				return _elm_lang$core$Native_Utils.eq(motion.velocity, 0) && _elm_lang$core$Native_Utils.eq(motion.position, motion.target);
			case 'Easing':
				return _elm_lang$core$Native_Utils.eq(_p23._0.progress, 1);
			default:
				return _elm_lang$core$Native_Utils.eq(motion.position, motion.target);
		}
	};
	var _p24 = property;
	switch (_p24.ctor) {
		case 'ExactProperty':
			return true;
		case 'ColorProperty':
			return A2(
				_elm_lang$core$List$all,
				motionDone,
				{
					ctor: '::',
					_0: _p24._1,
					_1: {
						ctor: '::',
						_0: _p24._2,
						_1: {
							ctor: '::',
							_0: _p24._3,
							_1: {
								ctor: '::',
								_0: _p24._4,
								_1: {ctor: '[]'}
							}
						}
					}
				});
		case 'ShadowProperty':
			var _p25 = _p24._2;
			return A2(
				_elm_lang$core$List$all,
				motionDone,
				{
					ctor: '::',
					_0: _p25.offsetX,
					_1: {
						ctor: '::',
						_0: _p25.offsetY,
						_1: {
							ctor: '::',
							_0: _p25.size,
							_1: {
								ctor: '::',
								_0: _p25.blur,
								_1: {
									ctor: '::',
									_0: _p25.red,
									_1: {
										ctor: '::',
										_0: _p25.green,
										_1: {
											ctor: '::',
											_0: _p25.blue,
											_1: {
												ctor: '::',
												_0: _p25.alpha,
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				});
		case 'Property':
			return motionDone(_p24._1);
		case 'Property2':
			return motionDone(_p24._1) && motionDone(_p24._2);
		case 'Property3':
			return A2(
				_elm_lang$core$List$all,
				motionDone,
				{
					ctor: '::',
					_0: _p24._1,
					_1: {
						ctor: '::',
						_0: _p24._2,
						_1: {
							ctor: '::',
							_0: _p24._3,
							_1: {ctor: '[]'}
						}
					}
				});
		case 'Property4':
			return A2(
				_elm_lang$core$List$all,
				motionDone,
				{
					ctor: '::',
					_0: _p24._1,
					_1: {
						ctor: '::',
						_0: _p24._2,
						_1: {
							ctor: '::',
							_0: _p24._3,
							_1: {
								ctor: '::',
								_0: _p24._4,
								_1: {ctor: '[]'}
							}
						}
					}
				});
		case 'AngleProperty':
			return motionDone(_p24._1);
		case 'Points':
			return A2(
				_elm_lang$core$List$all,
				function (_p26) {
					var _p27 = _p26;
					return motionDone(_p27._0) && motionDone(_p27._1);
				},
				_p24._0);
		default:
			return A2(_elm_lang$core$List$all, _mdgriffith$elm_style_animation$Animation_Model$isCmdDone, _p24._0);
	}
};
var _mdgriffith$elm_style_animation$Animation_Model$refreshTiming = F2(
	function (now, timing) {
		var dt = now - timing.current;
		return {
			current: now,
			dt: ((_elm_lang$core$Native_Utils.cmp(dt, 34) > 0) || _elm_lang$core$Native_Utils.eq(timing.current, 0)) ? 16.666 : dt
		};
	});
var _mdgriffith$elm_style_animation$Animation_Model$propertyName = function (prop) {
	var _p28 = prop;
	switch (_p28.ctor) {
		case 'ExactProperty':
			return _p28._0;
		case 'ColorProperty':
			return _p28._0;
		case 'ShadowProperty':
			return _p28._0;
		case 'Property':
			return _p28._0;
		case 'Property2':
			return _p28._0;
		case 'Property3':
			return _p28._0;
		case 'Property4':
			return _p28._0;
		case 'AngleProperty':
			return _p28._0;
		case 'Points':
			return 'points';
		default:
			return 'path';
	}
};
var _mdgriffith$elm_style_animation$Animation_Model$replaceProps = F2(
	function (props, replacements) {
		var replacementNames = A2(_elm_lang$core$List$map, _mdgriffith$elm_style_animation$Animation_Model$propertyName, replacements);
		var removed = A2(
			_elm_lang$core$List$filter,
			function (prop) {
				return !A2(
					_elm_lang$core$List$member,
					_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
					replacementNames);
			},
			props);
		return A2(_elm_lang$core$Basics_ops['++'], removed, replacements);
	});
var _mdgriffith$elm_style_animation$Animation_Model$zipPropertiesGreedy = F2(
	function (initialProps, newTargetProps) {
		var propertyMatch = F2(
			function (prop1, prop2) {
				return _elm_lang$core$Native_Utils.eq(
					_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop1),
					_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop2));
			});
		var _p29 = A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p31, _p30) {
					var _p32 = _p30;
					var _p39 = _p32._1;
					var _p38 = _p32._0;
					var _p37 = _p32._2;
					var _p33 = _elm_lang$core$List$head(_p38);
					if (_p33.ctor === 'Nothing') {
						return {ctor: '_Tuple3', _0: _p38, _1: _p39, _2: _p37};
					} else {
						var _p36 = _p33._0;
						var _p34 = A2(
							_elm_lang$core$List$partition,
							propertyMatch(_p36),
							_p39);
						var matchingBs = _p34._0;
						var nonMatchingBs = _p34._1;
						return {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$List$drop, 1, _p38),
							_1: function () {
								var _p35 = matchingBs;
								if (_p35.ctor === '[]') {
									return nonMatchingBs;
								} else {
									return A2(_elm_lang$core$Basics_ops['++'], _p35._1, nonMatchingBs);
								}
							}(),
							_2: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: _p36,
									_1: _elm_lang$core$List$head(matchingBs)
								},
								_1: _p37
							}
						};
					}
				}),
			{
				ctor: '_Tuple3',
				_0: initialProps,
				_1: newTargetProps,
				_2: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$List$repeat,
				_elm_lang$core$List$length(initialProps),
				0));
		var warnings = _p29._1;
		var props = _p29._2;
		var _p40 = A2(
			_elm_lang$core$List$map,
			function (b) {
				return A2(
					_elm_lang$core$Debug$log,
					'elm-style-animation',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_mdgriffith$elm_style_animation$Animation_Model$propertyName(b),
						' has no initial value and therefore will not be animated.'));
			},
			warnings);
		return _elm_lang$core$List$reverse(props);
	});
var _mdgriffith$elm_style_animation$Animation_Model$Timing = F2(
	function (a, b) {
		return {current: a, dt: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Motion = F6(
	function (a, b, c, d, e, f) {
		return {position: a, velocity: b, target: c, interpolation: d, unit: e, interpolationOverride: f};
	});
var _mdgriffith$elm_style_animation$Animation_Model$ShadowMotion = F8(
	function (a, b, c, d, e, f, g, h) {
		return {offsetX: a, offsetY: b, size: c, blur: d, red: e, green: f, blue: g, alpha: h};
	});
var _mdgriffith$elm_style_animation$Animation_Model$CubicCurveMotion = F3(
	function (a, b, c) {
		return {control1: a, control2: b, point: c};
	});
var _mdgriffith$elm_style_animation$Animation_Model$QuadraticCurveMotion = F2(
	function (a, b) {
		return {control: a, point: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$ArcMotion = F5(
	function (a, b, c, d, e) {
		return {x: a, y: b, radius: c, startAngle: d, endAngle: e};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Animation = function (a) {
	return {ctor: 'Animation', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Tick = function (a) {
	return {ctor: 'Tick', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Loop = function (a) {
	return {ctor: 'Loop', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Repeat = F2(
	function (a, b) {
		return {ctor: 'Repeat', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Send = function (a) {
	return {ctor: 'Send', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Wait = function (a) {
	return {ctor: 'Wait', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Set = function (a) {
	return {ctor: 'Set', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$ToWith = function (a) {
	return {ctor: 'ToWith', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$To = function (a) {
	return {ctor: 'To', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Step = {ctor: 'Step'};
var _mdgriffith$elm_style_animation$Animation_Model$AtSpeed = function (a) {
	return {ctor: 'AtSpeed', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Easing = function (a) {
	return {ctor: 'Easing', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$stepInterpolation = F2(
	function (dtms, motion) {
		var interpolationToUse = A2(_elm_lang$core$Maybe$withDefault, motion.interpolation, motion.interpolationOverride);
		var _p41 = interpolationToUse;
		switch (_p41.ctor) {
			case 'AtSpeed':
				var _p43 = _p41._0.perSecond;
				var _p42 = function () {
					if (_elm_lang$core$Native_Utils.cmp(motion.position, motion.target) < 0) {
						var $new = motion.position + (_p43 * (dtms / 1000));
						return {
							ctor: '_Tuple2',
							_0: $new,
							_1: _elm_lang$core$Native_Utils.cmp($new, motion.target) > -1
						};
					} else {
						var $new = motion.position - (_p43 * (dtms / 1000));
						return {
							ctor: '_Tuple2',
							_0: $new,
							_1: _elm_lang$core$Native_Utils.cmp($new, motion.target) < 1
						};
					}
				}();
				var newPos = _p42._0;
				var finished = _p42._1;
				return finished ? _elm_lang$core$Native_Utils.update(
					motion,
					{position: motion.target, velocity: 0.0}) : _elm_lang$core$Native_Utils.update(
					motion,
					{position: newPos, velocity: _p43 * 1000});
			case 'Spring':
				var fdamper = (-1 * _p41._0.damping) * motion.velocity;
				var fspring = _p41._0.stiffness * (motion.target - motion.position);
				var a = fspring + fdamper;
				var dt = dtms / 1000;
				var newVelocity = motion.velocity + (a * dt);
				var newPos = motion.position + (newVelocity * dt);
				var dx = _elm_lang$core$Basics$abs(motion.target - newPos);
				return ((_elm_lang$core$Native_Utils.cmp(dx, _mdgriffith$elm_style_animation$Animation_Model$tolerance) < 0) && (_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Basics$abs(newVelocity),
					_mdgriffith$elm_style_animation$Animation_Model$vTolerance) < 0)) ? _elm_lang$core$Native_Utils.update(
					motion,
					{position: motion.target, velocity: 0.0}) : _elm_lang$core$Native_Utils.update(
					motion,
					{position: newPos, velocity: newVelocity});
			default:
				var _p48 = _p41._0.start;
				var _p47 = _p41._0.progress;
				var _p46 = _p41._0.ease;
				var _p45 = _p41._0.duration;
				var distance = motion.target - _p48;
				var newProgress = (_elm_lang$core$Native_Utils.cmp((dtms / _p45) + _p47, 1) < 0) ? ((dtms / _p45) + _p47) : 1;
				var eased = _p46(newProgress);
				var newPos = (eased * distance) + _p48;
				var newVelocity = _elm_lang$core$Native_Utils.eq(newProgress, 1) ? 0 : ((newPos - motion.position) / dtms);
				var _p44 = motion.interpolationOverride;
				if (_p44.ctor === 'Nothing') {
					return _elm_lang$core$Native_Utils.update(
						motion,
						{
							position: newPos,
							velocity: newVelocity,
							interpolation: _mdgriffith$elm_style_animation$Animation_Model$Easing(
								{progress: newProgress, duration: _p45, ease: _p46, start: _p48})
						});
				} else {
					return _elm_lang$core$Native_Utils.update(
						motion,
						{
							position: newPos,
							velocity: newVelocity,
							interpolationOverride: _elm_lang$core$Maybe$Just(
								_mdgriffith$elm_style_animation$Animation_Model$Easing(
									{progress: newProgress, duration: _p45, ease: _p46, start: _p48}))
						});
				}
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$Spring = function (a) {
	return {ctor: 'Spring', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Path = function (a) {
	return {ctor: 'Path', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Points = function (a) {
	return {ctor: 'Points', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$AngleProperty = F2(
	function (a, b) {
		return {ctor: 'AngleProperty', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Property4 = F5(
	function (a, b, c, d, e) {
		return {ctor: 'Property4', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Property3 = F4(
	function (a, b, c, d) {
		return {ctor: 'Property3', _0: a, _1: b, _2: c, _3: d};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Property2 = F3(
	function (a, b, c) {
		return {ctor: 'Property2', _0: a, _1: b, _2: c};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Property = F2(
	function (a, b) {
		return {ctor: 'Property', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$ShadowProperty = F3(
	function (a, b, c) {
		return {ctor: 'ShadowProperty', _0: a, _1: b, _2: c};
	});
var _mdgriffith$elm_style_animation$Animation_Model$ColorProperty = F5(
	function (a, b, c, d, e) {
		return {ctor: 'ColorProperty', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _mdgriffith$elm_style_animation$Animation_Model$ExactProperty = F2(
	function (a, b) {
		return {ctor: 'ExactProperty', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Close = {ctor: 'Close'};
var _mdgriffith$elm_style_animation$Animation_Model$AntiClockwiseArc = function (a) {
	return {ctor: 'AntiClockwiseArc', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$ClockwiseArc = function (a) {
	return {ctor: 'ClockwiseArc', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$SmoothTo = function (a) {
	return {ctor: 'SmoothTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Smooth = function (a) {
	return {ctor: 'Smooth', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadraticTo = function (a) {
	return {ctor: 'SmoothQuadraticTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadratic = function (a) {
	return {ctor: 'SmoothQuadratic', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$QuadraticTo = function (a) {
	return {ctor: 'QuadraticTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Quadratic = function (a) {
	return {ctor: 'Quadratic', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$CurveTo = function (a) {
	return {ctor: 'CurveTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Curve = function (a) {
	return {ctor: 'Curve', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$VerticalTo = function (a) {
	return {ctor: 'VerticalTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Vertical = function (a) {
	return {ctor: 'Vertical', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$HorizontalTo = function (a) {
	return {ctor: 'HorizontalTo', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$Horizontal = function (a) {
	return {ctor: 'Horizontal', _0: a};
};
var _mdgriffith$elm_style_animation$Animation_Model$LineTo = F2(
	function (a, b) {
		return {ctor: 'LineTo', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Line = F2(
	function (a, b) {
		return {ctor: 'Line', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$MoveTo = F2(
	function (a, b) {
		return {ctor: 'MoveTo', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$Move = F2(
	function (a, b) {
		return {ctor: 'Move', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation_Model$mapPathMotion = F2(
	function (fn, cmd) {
		var mapCoords = function (coords) {
			return A2(
				_elm_lang$core$List$map,
				function (_p49) {
					var _p50 = _p49;
					return {
						ctor: '_Tuple2',
						_0: fn(_p50._0),
						_1: fn(_p50._1)
					};
				},
				coords);
		};
		var _p51 = cmd;
		switch (_p51.ctor) {
			case 'Move':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$Move,
					fn(_p51._0),
					fn(_p51._1));
			case 'MoveTo':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$MoveTo,
					fn(_p51._0),
					fn(_p51._1));
			case 'Line':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$Line,
					fn(_p51._0),
					fn(_p51._1));
			case 'LineTo':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$LineTo,
					fn(_p51._0),
					fn(_p51._1));
			case 'Horizontal':
				return _mdgriffith$elm_style_animation$Animation_Model$Horizontal(
					fn(_p51._0));
			case 'HorizontalTo':
				return _mdgriffith$elm_style_animation$Animation_Model$HorizontalTo(
					fn(_p51._0));
			case 'Vertical':
				return _mdgriffith$elm_style_animation$Animation_Model$Vertical(
					fn(_p51._0));
			case 'VerticalTo':
				return _mdgriffith$elm_style_animation$Animation_Model$VerticalTo(
					fn(_p51._0));
			case 'Curve':
				var _p54 = _p51._0.point;
				var _p53 = _p51._0.control2;
				var _p52 = _p51._0.control1;
				return _mdgriffith$elm_style_animation$Animation_Model$Curve(
					{
						control1: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p52)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p52))
						},
						control2: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p53)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p53))
						},
						point: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p54)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p54))
						}
					});
			case 'CurveTo':
				var _p57 = _p51._0.point;
				var _p56 = _p51._0.control2;
				var _p55 = _p51._0.control1;
				return _mdgriffith$elm_style_animation$Animation_Model$CurveTo(
					{
						control1: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p55)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p55))
						},
						control2: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p56)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p56))
						},
						point: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p57)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p57))
						}
					});
			case 'Quadratic':
				var _p59 = _p51._0.point;
				var _p58 = _p51._0.control;
				return _mdgriffith$elm_style_animation$Animation_Model$Quadratic(
					{
						control: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p58)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p58))
						},
						point: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p59)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p59))
						}
					});
			case 'QuadraticTo':
				var _p61 = _p51._0.point;
				var _p60 = _p51._0.control;
				return _mdgriffith$elm_style_animation$Animation_Model$QuadraticTo(
					{
						control: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p60)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p60))
						},
						point: {
							ctor: '_Tuple2',
							_0: fn(
								_elm_lang$core$Tuple$first(_p61)),
							_1: fn(
								_elm_lang$core$Tuple$second(_p61))
						}
					});
			case 'SmoothQuadratic':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadratic(
					mapCoords(_p51._0));
			case 'SmoothQuadraticTo':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadraticTo(
					mapCoords(_p51._0));
			case 'Smooth':
				return _mdgriffith$elm_style_animation$Animation_Model$Smooth(
					mapCoords(_p51._0));
			case 'SmoothTo':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothTo(
					mapCoords(_p51._0));
			case 'ClockwiseArc':
				var _p62 = _p51._0;
				return _mdgriffith$elm_style_animation$Animation_Model$ClockwiseArc(
					function () {
						var endAngle = _p62.endAngle;
						var startAngle = _p62.startAngle;
						var radius = _p62.radius;
						var y = _p62.y;
						var x = _p62.x;
						return _elm_lang$core$Native_Utils.update(
							_p62,
							{
								x: fn(x),
								y: fn(y),
								radius: fn(radius),
								startAngle: fn(startAngle),
								endAngle: fn(endAngle)
							});
					}());
			case 'AntiClockwiseArc':
				var _p63 = _p51._0;
				return _mdgriffith$elm_style_animation$Animation_Model$AntiClockwiseArc(
					function () {
						var endAngle = _p63.endAngle;
						var startAngle = _p63.startAngle;
						var radius = _p63.radius;
						var y = _p63.y;
						var x = _p63.x;
						return _elm_lang$core$Native_Utils.update(
							_p63,
							{
								x: fn(x),
								y: fn(y),
								radius: fn(radius),
								startAngle: fn(startAngle),
								endAngle: fn(endAngle)
							});
					}());
			default:
				return _mdgriffith$elm_style_animation$Animation_Model$Close;
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$mapToMotion = F2(
	function (fn, prop) {
		var _p64 = prop;
		switch (_p64.ctor) {
			case 'ExactProperty':
				return A2(_mdgriffith$elm_style_animation$Animation_Model$ExactProperty, _p64._0, _p64._1);
			case 'ColorProperty':
				return A5(
					_mdgriffith$elm_style_animation$Animation_Model$ColorProperty,
					_p64._0,
					fn(_p64._1),
					fn(_p64._2),
					fn(_p64._3),
					fn(_p64._4));
			case 'ShadowProperty':
				var _p65 = _p64._2;
				var alpha = _p65.alpha;
				var blue = _p65.blue;
				var green = _p65.green;
				var red = _p65.red;
				var blur = _p65.blur;
				var size = _p65.size;
				var offsetY = _p65.offsetY;
				var offsetX = _p65.offsetX;
				return A3(
					_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
					_p64._0,
					_p64._1,
					{
						offsetX: fn(offsetX),
						offsetY: fn(offsetY),
						size: fn(size),
						blur: fn(blur),
						red: fn(red),
						green: fn(green),
						blue: fn(blue),
						alpha: fn(alpha)
					});
			case 'Property':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$Property,
					_p64._0,
					fn(_p64._1));
			case 'Property2':
				return A3(
					_mdgriffith$elm_style_animation$Animation_Model$Property2,
					_p64._0,
					fn(_p64._1),
					fn(_p64._2));
			case 'Property3':
				return A4(
					_mdgriffith$elm_style_animation$Animation_Model$Property3,
					_p64._0,
					fn(_p64._1),
					fn(_p64._2),
					fn(_p64._3));
			case 'Property4':
				return A5(
					_mdgriffith$elm_style_animation$Animation_Model$Property4,
					_p64._0,
					fn(_p64._1),
					fn(_p64._2),
					fn(_p64._3),
					fn(_p64._4));
			case 'AngleProperty':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$AngleProperty,
					_p64._0,
					fn(_p64._1));
			case 'Points':
				return _mdgriffith$elm_style_animation$Animation_Model$Points(
					A2(
						_elm_lang$core$List$map,
						function (_p66) {
							var _p67 = _p66;
							return {
								ctor: '_Tuple2',
								_0: fn(_p67._0),
								_1: fn(_p67._1)
							};
						},
						_p64._0));
			default:
				return _mdgriffith$elm_style_animation$Animation_Model$Path(
					A2(
						_elm_lang$core$List$map,
						_mdgriffith$elm_style_animation$Animation_Model$mapPathMotion(fn),
						_p64._0));
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$setPathTarget = F2(
	function (cmd, targetCmd) {
		var setMotionTarget = F2(
			function (motion, targetMotion) {
				var _p68 = motion.interpolation;
				if (_p68.ctor === 'Easing') {
					return _elm_lang$core$Native_Utils.update(
						motion,
						{
							target: targetMotion.position,
							interpolation: _mdgriffith$elm_style_animation$Animation_Model$Easing(
								_elm_lang$core$Native_Utils.update(
									_p68._0,
									{start: motion.position}))
						});
				} else {
					return _elm_lang$core$Native_Utils.update(
						motion,
						{target: targetMotion.position});
				}
			});
		var _p69 = cmd;
		switch (_p69.ctor) {
			case 'Move':
				var _p70 = targetCmd;
				if (_p70.ctor === 'Move') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$Move,
						A2(setMotionTarget, _p69._0, _p70._0),
						A2(setMotionTarget, _p69._1, _p70._1));
				} else {
					return cmd;
				}
			case 'MoveTo':
				var _p71 = targetCmd;
				if (_p71.ctor === 'MoveTo') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$MoveTo,
						A2(setMotionTarget, _p69._0, _p71._0),
						A2(setMotionTarget, _p69._1, _p71._1));
				} else {
					return cmd;
				}
			case 'Line':
				var _p72 = targetCmd;
				if (_p72.ctor === 'Line') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$Line,
						A2(setMotionTarget, _p69._0, _p72._0),
						A2(setMotionTarget, _p69._1, _p72._1));
				} else {
					return cmd;
				}
			case 'LineTo':
				var _p73 = targetCmd;
				if (_p73.ctor === 'LineTo') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$LineTo,
						A2(setMotionTarget, _p69._0, _p73._0),
						A2(setMotionTarget, _p69._1, _p73._1));
				} else {
					return cmd;
				}
			case 'Horizontal':
				var _p74 = targetCmd;
				if (_p74.ctor === 'Horizontal') {
					return _mdgriffith$elm_style_animation$Animation_Model$Horizontal(
						A2(setMotionTarget, _p69._0, _p74._0));
				} else {
					return cmd;
				}
			case 'HorizontalTo':
				var _p75 = targetCmd;
				if (_p75.ctor === 'HorizontalTo') {
					return _mdgriffith$elm_style_animation$Animation_Model$HorizontalTo(
						A2(setMotionTarget, _p69._0, _p75._0));
				} else {
					return cmd;
				}
			case 'Vertical':
				var _p76 = targetCmd;
				if (_p76.ctor === 'Vertical') {
					return _mdgriffith$elm_style_animation$Animation_Model$Vertical(
						A2(setMotionTarget, _p69._0, _p76._0));
				} else {
					return cmd;
				}
			case 'VerticalTo':
				var _p77 = targetCmd;
				if (_p77.ctor === 'VerticalTo') {
					return _mdgriffith$elm_style_animation$Animation_Model$VerticalTo(
						A2(setMotionTarget, _p69._0, _p77._0));
				} else {
					return cmd;
				}
			case 'Curve':
				var _p80 = _p69._0;
				var _p78 = targetCmd;
				if (_p78.ctor === 'Curve') {
					var _p79 = _p78._0;
					return _mdgriffith$elm_style_animation$Animation_Model$Curve(
						{
							control1: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p80.control1),
									_elm_lang$core$Tuple$first(_p79.control1)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p80.control1),
									_elm_lang$core$Tuple$second(_p79.control1))
							},
							control2: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p80.control2),
									_elm_lang$core$Tuple$first(_p79.control2)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p80.control2),
									_elm_lang$core$Tuple$second(_p79.control2))
							},
							point: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p80.point),
									_elm_lang$core$Tuple$first(_p79.point)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p80.point),
									_elm_lang$core$Tuple$second(_p79.point))
							}
						});
				} else {
					return cmd;
				}
			case 'CurveTo':
				var _p83 = _p69._0;
				var _p81 = targetCmd;
				if (_p81.ctor === 'CurveTo') {
					var _p82 = _p81._0;
					return _mdgriffith$elm_style_animation$Animation_Model$CurveTo(
						{
							control1: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p83.control1),
									_elm_lang$core$Tuple$first(_p82.control1)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p83.control1),
									_elm_lang$core$Tuple$second(_p82.control1))
							},
							control2: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p83.control2),
									_elm_lang$core$Tuple$first(_p82.control2)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p83.control2),
									_elm_lang$core$Tuple$second(_p82.control2))
							},
							point: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p83.point),
									_elm_lang$core$Tuple$first(_p82.point)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p83.point),
									_elm_lang$core$Tuple$second(_p82.point))
							}
						});
				} else {
					return cmd;
				}
			case 'Quadratic':
				var _p86 = _p69._0;
				var _p84 = targetCmd;
				if (_p84.ctor === 'Quadratic') {
					var _p85 = _p84._0;
					return _mdgriffith$elm_style_animation$Animation_Model$Quadratic(
						{
							control: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p86.control),
									_elm_lang$core$Tuple$first(_p85.control)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p86.control),
									_elm_lang$core$Tuple$second(_p85.control))
							},
							point: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p86.point),
									_elm_lang$core$Tuple$first(_p85.point)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p86.point),
									_elm_lang$core$Tuple$second(_p85.point))
							}
						});
				} else {
					return cmd;
				}
			case 'QuadraticTo':
				var _p89 = _p69._0;
				var _p87 = targetCmd;
				if (_p87.ctor === 'QuadraticTo') {
					var _p88 = _p87._0;
					return _mdgriffith$elm_style_animation$Animation_Model$QuadraticTo(
						{
							control: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p89.control),
									_elm_lang$core$Tuple$first(_p88.control)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p89.control),
									_elm_lang$core$Tuple$second(_p88.control))
							},
							point: {
								ctor: '_Tuple2',
								_0: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$first(_p89.point),
									_elm_lang$core$Tuple$first(_p88.point)),
								_1: A2(
									setMotionTarget,
									_elm_lang$core$Tuple$second(_p89.point),
									_elm_lang$core$Tuple$second(_p88.point))
							}
						});
				} else {
					return cmd;
				}
			case 'SmoothQuadratic':
				var _p90 = targetCmd;
				if (_p90.ctor === 'SmoothQuadratic') {
					return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadratic(
						A3(
							_elm_lang$core$List$map2,
							F2(
								function (_p92, _p91) {
									var _p93 = _p92;
									var _p94 = _p91;
									return {
										ctor: '_Tuple2',
										_0: A2(setMotionTarget, _p93._0, _p94._0),
										_1: A2(setMotionTarget, _p93._1, _p94._1)
									};
								}),
							_p69._0,
							_p90._0));
				} else {
					return cmd;
				}
			case 'SmoothQuadraticTo':
				var _p95 = targetCmd;
				if (_p95.ctor === 'SmoothQuadraticTo') {
					return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadraticTo(
						A3(
							_elm_lang$core$List$map2,
							F2(
								function (_p97, _p96) {
									var _p98 = _p97;
									var _p99 = _p96;
									return {
										ctor: '_Tuple2',
										_0: A2(setMotionTarget, _p98._0, _p99._0),
										_1: A2(setMotionTarget, _p98._1, _p99._1)
									};
								}),
							_p69._0,
							_p95._0));
				} else {
					return cmd;
				}
			case 'Smooth':
				var _p100 = targetCmd;
				if (_p100.ctor === 'Smooth') {
					return _mdgriffith$elm_style_animation$Animation_Model$Smooth(
						A3(
							_elm_lang$core$List$map2,
							F2(
								function (_p102, _p101) {
									var _p103 = _p102;
									var _p104 = _p101;
									return {
										ctor: '_Tuple2',
										_0: A2(setMotionTarget, _p103._0, _p104._0),
										_1: A2(setMotionTarget, _p103._1, _p104._1)
									};
								}),
							_p69._0,
							_p100._0));
				} else {
					return cmd;
				}
			case 'SmoothTo':
				var _p105 = targetCmd;
				if (_p105.ctor === 'SmoothTo') {
					return _mdgriffith$elm_style_animation$Animation_Model$SmoothTo(
						A3(
							_elm_lang$core$List$map2,
							F2(
								function (_p107, _p106) {
									var _p108 = _p107;
									var _p109 = _p106;
									return {
										ctor: '_Tuple2',
										_0: A2(setMotionTarget, _p108._0, _p109._0),
										_1: A2(setMotionTarget, _p108._1, _p109._1)
									};
								}),
							_p69._0,
							_p105._0));
				} else {
					return cmd;
				}
			case 'ClockwiseArc':
				var _p112 = _p69._0;
				var _p110 = targetCmd;
				if (_p110.ctor === 'ClockwiseArc') {
					var _p111 = _p110._0;
					return _mdgriffith$elm_style_animation$Animation_Model$ClockwiseArc(
						function () {
							var endAngle = _p112.endAngle;
							var startAngle = _p112.startAngle;
							var radius = _p112.radius;
							var y = _p112.y;
							var x = _p112.x;
							return _elm_lang$core$Native_Utils.update(
								_p112,
								{
									x: A2(setMotionTarget, x, _p111.x),
									y: A2(setMotionTarget, y, _p111.y),
									radius: A2(setMotionTarget, radius, _p111.radius),
									startAngle: A2(setMotionTarget, startAngle, _p111.startAngle),
									endAngle: A2(setMotionTarget, endAngle, _p111.endAngle)
								});
						}());
				} else {
					return cmd;
				}
			case 'AntiClockwiseArc':
				var _p115 = _p69._0;
				var _p113 = targetCmd;
				if (_p113.ctor === 'AntiClockwiseArc') {
					var _p114 = _p113._0;
					return _mdgriffith$elm_style_animation$Animation_Model$AntiClockwiseArc(
						function () {
							var endAngle = _p115.endAngle;
							var startAngle = _p115.startAngle;
							var radius = _p115.radius;
							var y = _p115.y;
							var x = _p115.x;
							return _elm_lang$core$Native_Utils.update(
								_p115,
								{
									x: A2(setMotionTarget, x, _p114.x),
									y: A2(setMotionTarget, y, _p114.y),
									radius: A2(setMotionTarget, radius, _p114.radius),
									startAngle: A2(setMotionTarget, startAngle, _p114.startAngle),
									endAngle: A2(setMotionTarget, endAngle, _p114.endAngle)
								});
						}());
				} else {
					return cmd;
				}
			default:
				return _mdgriffith$elm_style_animation$Animation_Model$Close;
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$setTarget = F3(
	function (overrideInterpolation, current, newTarget) {
		var setMotionTarget = F2(
			function (motion, targetMotion) {
				var newMotion = overrideInterpolation ? _elm_lang$core$Native_Utils.update(
					motion,
					{
						interpolationOverride: _elm_lang$core$Maybe$Just(targetMotion.interpolation)
					}) : motion;
				var _p116 = newMotion.interpolationOverride;
				if (_p116.ctor === 'Nothing') {
					var _p117 = newMotion.interpolation;
					if (_p117.ctor === 'Easing') {
						return _elm_lang$core$Native_Utils.update(
							newMotion,
							{
								target: targetMotion.position,
								interpolation: _mdgriffith$elm_style_animation$Animation_Model$Easing(
									_elm_lang$core$Native_Utils.update(
										_p117._0,
										{start: motion.position, progress: 0}))
							});
					} else {
						return _elm_lang$core$Native_Utils.update(
							newMotion,
							{target: targetMotion.position});
					}
				} else {
					var _p118 = _p116._0;
					if (_p118.ctor === 'Easing') {
						return _elm_lang$core$Native_Utils.update(
							newMotion,
							{
								target: targetMotion.position,
								interpolationOverride: _elm_lang$core$Maybe$Just(
									_mdgriffith$elm_style_animation$Animation_Model$Easing(
										_elm_lang$core$Native_Utils.update(
											_p118._0,
											{start: motion.position, progress: 0})))
							});
					} else {
						return _elm_lang$core$Native_Utils.update(
							newMotion,
							{target: targetMotion.position});
					}
				}
			});
		var _p119 = current;
		switch (_p119.ctor) {
			case 'ExactProperty':
				return A2(_mdgriffith$elm_style_animation$Animation_Model$ExactProperty, _p119._0, _p119._1);
			case 'ColorProperty':
				var _p120 = newTarget;
				if (_p120.ctor === 'ColorProperty') {
					return A5(
						_mdgriffith$elm_style_animation$Animation_Model$ColorProperty,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p120._1),
						A2(setMotionTarget, _p119._2, _p120._2),
						A2(setMotionTarget, _p119._3, _p120._3),
						A2(setMotionTarget, _p119._4, _p120._4));
				} else {
					return current;
				}
			case 'ShadowProperty':
				var _p123 = _p119._2;
				var _p121 = newTarget;
				if (_p121.ctor === 'ShadowProperty') {
					var _p122 = _p121._2;
					return A3(
						_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
						_p119._0,
						_p119._1,
						{
							offsetX: A2(setMotionTarget, _p123.offsetX, _p122.offsetX),
							offsetY: A2(setMotionTarget, _p123.offsetY, _p122.offsetY),
							size: A2(setMotionTarget, _p123.size, _p122.size),
							blur: A2(setMotionTarget, _p123.blur, _p122.blur),
							red: A2(setMotionTarget, _p123.red, _p122.red),
							green: A2(setMotionTarget, _p123.green, _p122.green),
							blue: A2(setMotionTarget, _p123.blue, _p122.blue),
							alpha: A2(setMotionTarget, _p123.alpha, _p122.alpha)
						});
				} else {
					return current;
				}
			case 'Property':
				var _p124 = newTarget;
				if (_p124.ctor === 'Property') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$Property,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p124._1));
				} else {
					return current;
				}
			case 'Property2':
				var _p125 = newTarget;
				if (_p125.ctor === 'Property2') {
					return A3(
						_mdgriffith$elm_style_animation$Animation_Model$Property2,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p125._1),
						A2(setMotionTarget, _p119._2, _p125._2));
				} else {
					return current;
				}
			case 'Property3':
				var _p126 = newTarget;
				if (_p126.ctor === 'Property3') {
					return A4(
						_mdgriffith$elm_style_animation$Animation_Model$Property3,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p126._1),
						A2(setMotionTarget, _p119._2, _p126._2),
						A2(setMotionTarget, _p119._3, _p126._3));
				} else {
					return current;
				}
			case 'Property4':
				var _p127 = newTarget;
				if (_p127.ctor === 'Property4') {
					return A5(
						_mdgriffith$elm_style_animation$Animation_Model$Property4,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p127._1),
						A2(setMotionTarget, _p119._2, _p127._2),
						A2(setMotionTarget, _p119._3, _p127._3),
						A2(setMotionTarget, _p119._4, _p127._4));
				} else {
					return current;
				}
			case 'AngleProperty':
				var _p128 = newTarget;
				if (_p128.ctor === 'AngleProperty') {
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$AngleProperty,
						_p119._0,
						A2(setMotionTarget, _p119._1, _p128._1));
				} else {
					return current;
				}
			case 'Points':
				var _p129 = newTarget;
				if (_p129.ctor === 'Points') {
					var _p130 = A2(_mdgriffith$elm_style_animation$Animation_Model$matchPoints, _p119._0, _p129._0);
					var m1s = _p130._0;
					var m2s = _p130._1;
					return _mdgriffith$elm_style_animation$Animation_Model$Points(
						A3(
							_elm_lang$core$List$map2,
							F2(
								function (_p132, _p131) {
									var _p133 = _p132;
									var _p134 = _p131;
									return {
										ctor: '_Tuple2',
										_0: A2(setMotionTarget, _p133._0, _p134._0),
										_1: A2(setMotionTarget, _p133._1, _p134._1)
									};
								}),
							m1s,
							m2s));
				} else {
					return current;
				}
			default:
				var _p135 = newTarget;
				if (_p135.ctor === 'Path') {
					return _mdgriffith$elm_style_animation$Animation_Model$Path(
						A3(_elm_lang$core$List$map2, _mdgriffith$elm_style_animation$Animation_Model$setPathTarget, _p119._0, _p135._0));
				} else {
					return current;
				}
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$startTowards = F3(
	function (overrideInterpolation, current, target) {
		return A2(
			_elm_lang$core$List$filterMap,
			function (propPair) {
				var _p136 = propPair;
				if (_p136._1.ctor === 'Just') {
					return _elm_lang$core$Maybe$Just(
						A3(_mdgriffith$elm_style_animation$Animation_Model$setTarget, overrideInterpolation, _p136._0, _p136._1._0));
				} else {
					return _elm_lang$core$Maybe$Just(_p136._0);
				}
			},
			A2(_mdgriffith$elm_style_animation$Animation_Model$zipPropertiesGreedy, current, target));
	});
var _mdgriffith$elm_style_animation$Animation_Model$stepPath = F2(
	function (dt, cmd) {
		var stepCoords = function (coords) {
			return A2(
				_elm_lang$core$List$map,
				function (_p137) {
					var _p138 = _p137;
					return {
						ctor: '_Tuple2',
						_0: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p138._0),
						_1: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p138._1)
					};
				},
				coords);
		};
		var _p139 = cmd;
		switch (_p139.ctor) {
			case 'Move':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$Move,
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0),
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._1));
			case 'MoveTo':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$MoveTo,
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0),
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._1));
			case 'Line':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$Line,
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0),
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._1));
			case 'LineTo':
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$LineTo,
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0),
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._1));
			case 'Horizontal':
				return _mdgriffith$elm_style_animation$Animation_Model$Horizontal(
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0));
			case 'HorizontalTo':
				return _mdgriffith$elm_style_animation$Animation_Model$HorizontalTo(
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0));
			case 'Vertical':
				return _mdgriffith$elm_style_animation$Animation_Model$Vertical(
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0));
			case 'VerticalTo':
				return _mdgriffith$elm_style_animation$Animation_Model$VerticalTo(
					A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p139._0));
			case 'Curve':
				var _p142 = _p139._0.point;
				var _p141 = _p139._0.control2;
				var _p140 = _p139._0.control1;
				return _mdgriffith$elm_style_animation$Animation_Model$Curve(
					{
						control1: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p140)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p140))
						},
						control2: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p141)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p141))
						},
						point: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p142)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p142))
						}
					});
			case 'CurveTo':
				var _p145 = _p139._0.point;
				var _p144 = _p139._0.control2;
				var _p143 = _p139._0.control1;
				return _mdgriffith$elm_style_animation$Animation_Model$CurveTo(
					{
						control1: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p143)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p143))
						},
						control2: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p144)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p144))
						},
						point: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p145)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p145))
						}
					});
			case 'Quadratic':
				var _p147 = _p139._0.point;
				var _p146 = _p139._0.control;
				return _mdgriffith$elm_style_animation$Animation_Model$Quadratic(
					{
						control: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p146)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p146))
						},
						point: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p147)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p147))
						}
					});
			case 'QuadraticTo':
				var _p149 = _p139._0.point;
				var _p148 = _p139._0.control;
				return _mdgriffith$elm_style_animation$Animation_Model$QuadraticTo(
					{
						control: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p148)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p148))
						},
						point: {
							ctor: '_Tuple2',
							_0: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$first(_p149)),
							_1: A2(
								_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation,
								dt,
								_elm_lang$core$Tuple$second(_p149))
						}
					});
			case 'SmoothQuadratic':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadratic(
					stepCoords(_p139._0));
			case 'SmoothQuadraticTo':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothQuadraticTo(
					stepCoords(_p139._0));
			case 'Smooth':
				return _mdgriffith$elm_style_animation$Animation_Model$Smooth(
					stepCoords(_p139._0));
			case 'SmoothTo':
				return _mdgriffith$elm_style_animation$Animation_Model$SmoothTo(
					stepCoords(_p139._0));
			case 'ClockwiseArc':
				var _p150 = _p139._0;
				return _mdgriffith$elm_style_animation$Animation_Model$ClockwiseArc(
					_elm_lang$core$Native_Utils.update(
						_p150,
						{
							x: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p150.x),
							y: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p150.y),
							radius: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p150.radius),
							startAngle: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p150.startAngle),
							endAngle: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p150.endAngle)
						}));
			case 'AntiClockwiseArc':
				var _p151 = _p139._0;
				return _mdgriffith$elm_style_animation$Animation_Model$AntiClockwiseArc(
					_elm_lang$core$Native_Utils.update(
						_p151,
						{
							x: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p151.x),
							y: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p151.y),
							radius: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p151.radius),
							startAngle: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p151.startAngle),
							endAngle: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p151.endAngle)
						}));
			default:
				return _mdgriffith$elm_style_animation$Animation_Model$Close;
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$step = F2(
	function (dt, props) {
		var stepProp = function (property) {
			var _p152 = property;
			switch (_p152.ctor) {
				case 'ExactProperty':
					return A2(_mdgriffith$elm_style_animation$Animation_Model$ExactProperty, _p152._0, _p152._1);
				case 'Property':
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$Property,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1));
				case 'Property2':
					return A3(
						_mdgriffith$elm_style_animation$Animation_Model$Property2,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._2));
				case 'Property3':
					return A4(
						_mdgriffith$elm_style_animation$Animation_Model$Property3,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._2),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._3));
				case 'Property4':
					return A5(
						_mdgriffith$elm_style_animation$Animation_Model$Property4,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._2),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._3),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._4));
				case 'AngleProperty':
					return A2(
						_mdgriffith$elm_style_animation$Animation_Model$AngleProperty,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1));
				case 'ColorProperty':
					return A5(
						_mdgriffith$elm_style_animation$Animation_Model$ColorProperty,
						_p152._0,
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._1),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._2),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._3),
						A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p152._4));
				case 'ShadowProperty':
					var _p153 = _p152._2;
					return A3(
						_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
						_p152._0,
						_p152._1,
						{
							offsetX: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.offsetX),
							offsetY: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.offsetY),
							size: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.size),
							blur: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.blur),
							red: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.red),
							green: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.green),
							blue: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.blue),
							alpha: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p153.alpha)
						});
				case 'Points':
					return _mdgriffith$elm_style_animation$Animation_Model$Points(
						A2(
							_elm_lang$core$List$map,
							function (_p154) {
								var _p155 = _p154;
								return {
									ctor: '_Tuple2',
									_0: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p155._0),
									_1: A2(_mdgriffith$elm_style_animation$Animation_Model$stepInterpolation, dt, _p155._1)
								};
							},
							_p152._0));
				default:
					return _mdgriffith$elm_style_animation$Animation_Model$Path(
						A2(
							_elm_lang$core$List$map,
							_mdgriffith$elm_style_animation$Animation_Model$stepPath(dt),
							_p152._0));
			}
		};
		return A2(_elm_lang$core$List$map, stepProp, props);
	});
var _mdgriffith$elm_style_animation$Animation_Model$resolveSteps = F3(
	function (currentStyle, steps, dt) {
		resolveSteps:
		while (true) {
			var _p156 = _elm_lang$core$List$head(steps);
			if (_p156.ctor === 'Nothing') {
				return {
					ctor: '_Tuple3',
					_0: currentStyle,
					_1: {ctor: '[]'},
					_2: {ctor: '[]'}
				};
			} else {
				var _p157 = _p156._0;
				switch (_p157.ctor) {
					case 'Wait':
						var _p158 = _p157._0;
						if (_elm_lang$core$Native_Utils.cmp(_p158, 0) < 1) {
							var _v70 = currentStyle,
								_v71 = A2(_elm_lang$core$List$drop, 1, steps),
								_v72 = dt;
							currentStyle = _v70;
							steps = _v71;
							dt = _v72;
							continue resolveSteps;
						} else {
							return {
								ctor: '_Tuple3',
								_0: currentStyle,
								_1: {ctor: '[]'},
								_2: {
									ctor: '::',
									_0: _mdgriffith$elm_style_animation$Animation_Model$Wait(_p158 - dt),
									_1: A2(_elm_lang$core$List$drop, 1, steps)
								}
							};
						}
					case 'Send':
						var _p159 = A3(
							_mdgriffith$elm_style_animation$Animation_Model$resolveSteps,
							currentStyle,
							A2(_elm_lang$core$List$drop, 1, steps),
							dt);
						var newStyle = _p159._0;
						var msgs = _p159._1;
						var remainingSteps = _p159._2;
						return {
							ctor: '_Tuple3',
							_0: newStyle,
							_1: {ctor: '::', _0: _p157._0, _1: msgs},
							_2: remainingSteps
						};
					case 'To':
						var _v73 = A3(_mdgriffith$elm_style_animation$Animation_Model$startTowards, false, currentStyle, _p157._0),
							_v74 = {
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation_Model$Step,
							_1: A2(_elm_lang$core$List$drop, 1, steps)
						},
							_v75 = dt;
						currentStyle = _v73;
						steps = _v74;
						dt = _v75;
						continue resolveSteps;
					case 'ToWith':
						var _v76 = A3(_mdgriffith$elm_style_animation$Animation_Model$startTowards, true, currentStyle, _p157._0),
							_v77 = {
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation_Model$Step,
							_1: A2(_elm_lang$core$List$drop, 1, steps)
						},
							_v78 = dt;
						currentStyle = _v76;
						steps = _v77;
						dt = _v78;
						continue resolveSteps;
					case 'Set':
						var _v79 = A2(_mdgriffith$elm_style_animation$Animation_Model$replaceProps, currentStyle, _p157._0),
							_v80 = A2(_elm_lang$core$List$drop, 1, steps),
							_v81 = dt;
						currentStyle = _v79;
						steps = _v80;
						dt = _v81;
						continue resolveSteps;
					case 'Step':
						var stepped = A2(_mdgriffith$elm_style_animation$Animation_Model$step, dt, currentStyle);
						return A2(_elm_lang$core$List$all, _mdgriffith$elm_style_animation$Animation_Model$isDone, stepped) ? {
							ctor: '_Tuple3',
							_0: A2(
								_elm_lang$core$List$map,
								_mdgriffith$elm_style_animation$Animation_Model$mapToMotion(
									function (m) {
										return _elm_lang$core$Native_Utils.update(
											m,
											{interpolationOverride: _elm_lang$core$Maybe$Nothing});
									}),
								stepped),
							_1: {ctor: '[]'},
							_2: A2(_elm_lang$core$List$drop, 1, steps)
						} : {
							ctor: '_Tuple3',
							_0: stepped,
							_1: {ctor: '[]'},
							_2: steps
						};
					case 'Loop':
						var _p160 = _p157._0;
						var _v82 = currentStyle,
							_v83 = A2(
							_elm_lang$core$Basics_ops['++'],
							_p160,
							{
								ctor: '::',
								_0: _mdgriffith$elm_style_animation$Animation_Model$Loop(_p160),
								_1: {ctor: '[]'}
							}),
							_v84 = dt;
						currentStyle = _v82;
						steps = _v83;
						dt = _v84;
						continue resolveSteps;
					default:
						var _p162 = _p157._1;
						var _p161 = _p157._0;
						if (_elm_lang$core$Native_Utils.eq(_p161, 0)) {
							return {
								ctor: '_Tuple3',
								_0: currentStyle,
								_1: {ctor: '[]'},
								_2: A2(_elm_lang$core$List$drop, 1, _p162)
							};
						} else {
							var _v85 = currentStyle,
								_v86 = A2(
								_elm_lang$core$Basics_ops['++'],
								_p162,
								{
									ctor: '::',
									_0: A2(_mdgriffith$elm_style_animation$Animation_Model$Repeat, _p161 - 1, _p162),
									_1: {ctor: '[]'}
								}),
								_v87 = dt;
							currentStyle = _v85;
							steps = _v86;
							dt = _v87;
							continue resolveSteps;
						}
				}
			}
		}
	});
var _mdgriffith$elm_style_animation$Animation_Model$updateAnimation = F2(
	function (_p164, _p163) {
		var _p165 = _p164;
		var _p166 = _p163;
		var _p175 = _p166._0;
		var timing = A2(_mdgriffith$elm_style_animation$Animation_Model$refreshTiming, _p165._0, _p175.timing);
		var _p167 = A2(
			_elm_lang$core$List$partition,
			function (_p168) {
				var _p169 = _p168;
				return _elm_lang$core$Native_Utils.cmp(_p169._0, 0) < 1;
			},
			A2(
				_elm_lang$core$List$map,
				function (_p170) {
					var _p171 = _p170;
					return {ctor: '_Tuple2', _0: _p171._0 - timing.dt, _1: _p171._1};
				},
				_p175.interruption));
		var readyInterruption = _p167._0;
		var queuedInterruptions = _p167._1;
		var _p172 = function () {
			var _p173 = _elm_lang$core$List$head(readyInterruption);
			if (_p173.ctor === 'Just') {
				return {
					ctor: '_Tuple2',
					_0: _p173._0._1,
					_1: A2(
						_elm_lang$core$List$map,
						_mdgriffith$elm_style_animation$Animation_Model$mapToMotion(
							function (m) {
								return _elm_lang$core$Native_Utils.update(
									m,
									{interpolationOverride: _elm_lang$core$Maybe$Nothing});
							}),
						_p175.style)
				};
			} else {
				return {ctor: '_Tuple2', _0: _p175.steps, _1: _p175.style};
			}
		}();
		var steps = _p172._0;
		var style = _p172._1;
		var _p174 = A3(_mdgriffith$elm_style_animation$Animation_Model$resolveSteps, style, steps, timing.dt);
		var revisedStyle = _p174._0;
		var sentMessages = _p174._1;
		var revisedSteps = _p174._2;
		return {
			ctor: '_Tuple2',
			_0: _mdgriffith$elm_style_animation$Animation_Model$Animation(
				_elm_lang$core$Native_Utils.update(
					_p175,
					{
						timing: timing,
						interruption: queuedInterruptions,
						running: (!_elm_lang$core$Native_Utils.eq(
							_elm_lang$core$List$length(revisedSteps),
							0)) || (!_elm_lang$core$Native_Utils.eq(
							_elm_lang$core$List$length(queuedInterruptions),
							0)),
						steps: revisedSteps,
						style: revisedStyle
					})),
			_1: _elm_lang$core$Platform_Cmd$batch(
				A2(
					_elm_lang$core$List$map,
					function (m) {
						return A2(
							_elm_lang$core$Task$perform,
							_elm_lang$core$Basics$identity,
							_elm_lang$core$Task$succeed(m));
					},
					sentMessages))
		};
	});

var _mdgriffith$elm_style_animation$Animation$pathCmdValue = function (cmd) {
	var renderPoints = function (coords) {
		return A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				function (_p0) {
					var _p1 = _p0;
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p1._0.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							_elm_lang$core$Basics$toString(_p1._1.position)));
				},
				coords));
	};
	var _p2 = cmd;
	switch (_p2.ctor) {
		case 'Move':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'm ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p2._0.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						',',
						_elm_lang$core$Basics$toString(_p2._1.position))));
		case 'MoveTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'M ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p2._0.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						',',
						_elm_lang$core$Basics$toString(_p2._1.position))));
		case 'Line':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'l ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p2._0.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						',',
						_elm_lang$core$Basics$toString(_p2._1.position))));
		case 'LineTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'L ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p2._0.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						',',
						_elm_lang$core$Basics$toString(_p2._1.position))));
		case 'Horizontal':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'h ',
				_elm_lang$core$Basics$toString(_p2._0.position));
		case 'HorizontalTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'H ',
				_elm_lang$core$Basics$toString(_p2._0.position));
		case 'Vertical':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'v ',
				_elm_lang$core$Basics$toString(_p2._0.position));
		case 'VerticalTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'V ',
				_elm_lang$core$Basics$toString(_p2._0.position));
		case 'Curve':
			var _p3 = _p2._0.point;
			var p1x = _p3._0;
			var p1y = _p3._1;
			var _p4 = _p2._0.control2;
			var c2x = _p4._0;
			var c2y = _p4._1;
			var _p5 = _p2._0.control1;
			var c1x = _p5._0;
			var c1y = _p5._1;
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'c ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c1x.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(c1y.position),
							A2(
								_elm_lang$core$Basics_ops['++'],
								', ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(c2x.position),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(c2y.position),
											A2(
												_elm_lang$core$Basics_ops['++'],
												', ',
												A2(
													_elm_lang$core$Basics_ops['++'],
													_elm_lang$core$Basics$toString(p1x.position),
													A2(
														_elm_lang$core$Basics_ops['++'],
														' ',
														_elm_lang$core$Basics$toString(p1y.position))))))))))));
		case 'CurveTo':
			var _p6 = _p2._0.point;
			var p1x = _p6._0;
			var p1y = _p6._1;
			var _p7 = _p2._0.control2;
			var c2x = _p7._0;
			var c2y = _p7._1;
			var _p8 = _p2._0.control1;
			var c1x = _p8._0;
			var c1y = _p8._1;
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'C ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c1x.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(c1y.position),
							A2(
								_elm_lang$core$Basics_ops['++'],
								', ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(c2x.position),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(c2y.position),
											A2(
												_elm_lang$core$Basics_ops['++'],
												', ',
												A2(
													_elm_lang$core$Basics_ops['++'],
													_elm_lang$core$Basics$toString(p1x.position),
													A2(
														_elm_lang$core$Basics_ops['++'],
														' ',
														_elm_lang$core$Basics$toString(p1y.position))))))))))));
		case 'Quadratic':
			var _p9 = _p2._0.point;
			var p1x = _p9._0;
			var p1y = _p9._1;
			var _p10 = _p2._0.control;
			var c1x = _p10._0;
			var c1y = _p10._1;
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'q ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c1x.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(c1y.position),
							A2(
								_elm_lang$core$Basics_ops['++'],
								', ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(p1x.position),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										_elm_lang$core$Basics$toString(p1y.position))))))));
		case 'QuadraticTo':
			var _p11 = _p2._0.point;
			var p1x = _p11._0;
			var p1y = _p11._1;
			var _p12 = _p2._0.control;
			var c1x = _p12._0;
			var c1y = _p12._1;
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'Q ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c1x.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(c1y.position),
							A2(
								_elm_lang$core$Basics_ops['++'],
								', ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(p1x.position),
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										_elm_lang$core$Basics$toString(p1y.position))))))));
		case 'SmoothQuadratic':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				't ',
				renderPoints(_p2._0));
		case 'SmoothQuadraticTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'T ',
				renderPoints(_p2._0));
		case 'Smooth':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				's ',
				renderPoints(_p2._0));
		case 'SmoothTo':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'S ',
				renderPoints(_p2._0));
		case 'ClockwiseArc':
			var _p13 = _p2._0;
			var deltaAngle = _p13.endAngle.position - _p13.startAngle.position;
			if (_elm_lang$core$Native_Utils.cmp(deltaAngle, 360 - 1.0e-6) > 0) {
				var dy = _p13.radius.position * _elm_lang$core$Basics$sin(
					_elm_lang$core$Basics$degrees(_p13.startAngle.position));
				var dx = _p13.radius.position * _elm_lang$core$Basics$cos(
					_elm_lang$core$Basics$degrees(_p13.startAngle.position));
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'A ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p13.radius.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p13.radius.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',0,1,1,',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(_p13.x.position - dx),
										A2(
											_elm_lang$core$Basics_ops['++'],
											',',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p13.y.position - dy),
												A2(
													_elm_lang$core$Basics_ops['++'],
													' A ',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(_p13.radius.position),
														A2(
															_elm_lang$core$Basics_ops['++'],
															',',
															A2(
																_elm_lang$core$Basics_ops['++'],
																_elm_lang$core$Basics$toString(_p13.radius.position),
																A2(
																	_elm_lang$core$Basics_ops['++'],
																	',0,1,1,',
																	A2(
																		_elm_lang$core$Basics_ops['++'],
																		_elm_lang$core$Basics$toString(_p13.x.position + dx),
																		A2(
																			_elm_lang$core$Basics_ops['++'],
																			',',
																			_elm_lang$core$Basics$toString(_p13.y.position + dy))))))))))))))));
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'A ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p13.radius.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p13.radius.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									' 0 ',
									A2(
										_elm_lang$core$Basics_ops['++'],
										(_elm_lang$core$Native_Utils.cmp(deltaAngle, 180) > -1) ? '1' : '0',
										A2(
											_elm_lang$core$Basics_ops['++'],
											' ',
											A2(
												_elm_lang$core$Basics_ops['++'],
												'1',
												A2(
													_elm_lang$core$Basics_ops['++'],
													' ',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(
															_p13.x.position + (_p13.radius.position * _elm_lang$core$Basics$cos(
																_elm_lang$core$Basics$degrees(_p13.endAngle.position)))),
														A2(
															_elm_lang$core$Basics_ops['++'],
															',',
															_elm_lang$core$Basics$toString(
																_p13.y.position + (_p13.radius.position * _elm_lang$core$Basics$sin(
																	_elm_lang$core$Basics$degrees(_p13.endAngle.position)))))))))))))));
			}
		case 'AntiClockwiseArc':
			var _p14 = _p2._0;
			var deltaAngle = _p14.endAngle.position - _p14.startAngle.position;
			if (_elm_lang$core$Native_Utils.cmp(deltaAngle, 360 - 1.0e-6) > 0) {
				var dy = _p14.radius.position * _elm_lang$core$Basics$sin(
					_elm_lang$core$Basics$degrees(_p14.startAngle.position));
				var dx = _p14.radius.position * _elm_lang$core$Basics$cos(
					_elm_lang$core$Basics$degrees(_p14.startAngle.position));
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'A ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p14.radius.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p14.radius.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',0,1,0,',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(_p14.x.position - dx),
										A2(
											_elm_lang$core$Basics_ops['++'],
											',',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p14.y.position - dy),
												A2(
													_elm_lang$core$Basics_ops['++'],
													' A ',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(_p14.radius.position),
														A2(
															_elm_lang$core$Basics_ops['++'],
															',',
															A2(
																_elm_lang$core$Basics_ops['++'],
																_elm_lang$core$Basics$toString(_p14.radius.position),
																A2(
																	_elm_lang$core$Basics_ops['++'],
																	',0,1,1,',
																	A2(
																		_elm_lang$core$Basics_ops['++'],
																		_elm_lang$core$Basics$toString(_p14.x.position + dx),
																		A2(
																			_elm_lang$core$Basics_ops['++'],
																			',',
																			_elm_lang$core$Basics$toString(_p14.y.position + dy))))))))))))))));
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'A ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p14.radius.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p14.radius.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									' 0 ',
									A2(
										_elm_lang$core$Basics_ops['++'],
										(_elm_lang$core$Native_Utils.cmp(_p14.startAngle.position - _p14.endAngle.position, 180) > -1) ? '1' : '0',
										A2(
											_elm_lang$core$Basics_ops['++'],
											' ',
											A2(
												_elm_lang$core$Basics_ops['++'],
												'0',
												A2(
													_elm_lang$core$Basics_ops['++'],
													' ',
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(
															_p14.x.position + (_p14.radius.position * _elm_lang$core$Basics$cos(_p14.endAngle.position))),
														A2(
															_elm_lang$core$Basics_ops['++'],
															',',
															_elm_lang$core$Basics$toString(
																_p14.y.position + (_p14.radius.position * _elm_lang$core$Basics$sin(_p14.endAngle.position))))))))))))));
			}
		default:
			return 'z';
	}
};
var _mdgriffith$elm_style_animation$Animation$propertyValue = F2(
	function (prop, delim) {
		var _p15 = prop;
		switch (_p15.ctor) {
			case 'ExactProperty':
				return _p15._1;
			case 'ColorProperty':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'rgba(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(
							_elm_lang$core$Basics$round(_p15._1.position)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							',',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Basics$round(_p15._2.position)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(
											_elm_lang$core$Basics$round(_p15._3.position)),
										A2(
											_elm_lang$core$Basics_ops['++'],
											',',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p15._4.position),
												')'))))))));
			case 'ShadowProperty':
				var _p17 = _p15._2;
				var _p16 = _p15._0;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p15._1 ? 'inset ' : '',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p17.offsetX.position),
						A2(
							_elm_lang$core$Basics_ops['++'],
							'px',
							A2(
								_elm_lang$core$Basics_ops['++'],
								' ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_p17.offsetY.position),
									A2(
										_elm_lang$core$Basics_ops['++'],
										'px',
										A2(
											_elm_lang$core$Basics_ops['++'],
											' ',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p17.blur.position),
												A2(
													_elm_lang$core$Basics_ops['++'],
													'px',
													A2(
														_elm_lang$core$Basics_ops['++'],
														' ',
														A2(
															_elm_lang$core$Basics_ops['++'],
															(_elm_lang$core$Native_Utils.eq(_p16, 'text-shadow') || _elm_lang$core$Native_Utils.eq(_p16, 'drop-shadow')) ? '' : A2(
																_elm_lang$core$Basics_ops['++'],
																_elm_lang$core$Basics$toString(_p17.size.position),
																A2(_elm_lang$core$Basics_ops['++'], 'px', ' ')),
															A2(
																_elm_lang$core$Basics_ops['++'],
																'rgba(',
																A2(
																	_elm_lang$core$Basics_ops['++'],
																	_elm_lang$core$Basics$toString(
																		_elm_lang$core$Basics$round(_p17.red.position)),
																	A2(
																		_elm_lang$core$Basics_ops['++'],
																		', ',
																		A2(
																			_elm_lang$core$Basics_ops['++'],
																			_elm_lang$core$Basics$toString(
																				_elm_lang$core$Basics$round(_p17.green.position)),
																			A2(
																				_elm_lang$core$Basics_ops['++'],
																				', ',
																				A2(
																					_elm_lang$core$Basics_ops['++'],
																					_elm_lang$core$Basics$toString(
																						_elm_lang$core$Basics$round(_p17.blue.position)),
																					A2(
																						_elm_lang$core$Basics_ops['++'],
																						', ',
																						A2(
																							_elm_lang$core$Basics_ops['++'],
																							_elm_lang$core$Basics$toString(_p17.alpha.position),
																							')')))))))))))))))))));
			case 'Property':
				var _p18 = _p15._1;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p18.position),
					_p18.unit);
			case 'Property2':
				var _p20 = _p15._2;
				var _p19 = _p15._1;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p19.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p19.unit,
						A2(
							_elm_lang$core$Basics_ops['++'],
							delim,
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p20.position),
								_p20.unit))));
			case 'Property3':
				var _p23 = _p15._3;
				var _p22 = _p15._2;
				var _p21 = _p15._1;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p21.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p21.unit,
						A2(
							_elm_lang$core$Basics_ops['++'],
							delim,
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p22.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									_p22.unit,
									A2(
										_elm_lang$core$Basics_ops['++'],
										delim,
										A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(_p23.position),
											_p23.unit)))))));
			case 'Property4':
				var _p27 = _p15._4;
				var _p26 = _p15._3;
				var _p25 = _p15._2;
				var _p24 = _p15._1;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p24.position),
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p24.unit,
						A2(
							_elm_lang$core$Basics_ops['++'],
							delim,
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p25.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									_p25.unit,
									A2(
										_elm_lang$core$Basics_ops['++'],
										delim,
										A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(_p26.position),
											A2(
												_elm_lang$core$Basics_ops['++'],
												_p26.unit,
												A2(
													_elm_lang$core$Basics_ops['++'],
													delim,
													A2(
														_elm_lang$core$Basics_ops['++'],
														_elm_lang$core$Basics$toString(_p27.position),
														_p27.unit))))))))));
			case 'AngleProperty':
				var _p28 = _p15._1;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p28.position),
					_p28.unit);
			case 'Points':
				return A2(
					_elm_lang$core$String$join,
					' ',
					A2(
						_elm_lang$core$List$map,
						function (_p29) {
							var _p30 = _p29;
							return A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p30._0.position),
								A2(
									_elm_lang$core$Basics_ops['++'],
									',',
									_elm_lang$core$Basics$toString(_p30._1.position)));
						},
						_p15._0));
			default:
				return A2(
					_elm_lang$core$String$join,
					' ',
					A2(_elm_lang$core$List$map, _mdgriffith$elm_style_animation$Animation$pathCmdValue, _p15._0));
		}
	});
var _mdgriffith$elm_style_animation$Animation$displayModeName = function (mode) {
	var _p31 = mode;
	switch (_p31.ctor) {
		case 'None':
			return 'none';
		case 'Inline':
			return 'inline';
		case 'InlineBlock':
			return 'inline-block';
		case 'Block':
			return 'block';
		case 'Flex':
			return 'flex';
		case 'InlineFlex':
			return 'inline-flex';
		default:
			return 'list-item';
	}
};
var _mdgriffith$elm_style_animation$Animation$isAttr = function (prop) {
	return A2(
		_elm_lang$core$String$startsWith,
		'attr:',
		_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop)) || function () {
		var _p32 = prop;
		switch (_p32.ctor) {
			case 'Points':
				return true;
			case 'Path':
				return true;
			case 'Property':
				var _p33 = _p32._0;
				return _elm_lang$core$Native_Utils.eq(_p33, 'cx') || (_elm_lang$core$Native_Utils.eq(_p33, 'cy') || (_elm_lang$core$Native_Utils.eq(_p33, 'x') || (_elm_lang$core$Native_Utils.eq(_p33, 'y') || (_elm_lang$core$Native_Utils.eq(_p33, 'rx') || (_elm_lang$core$Native_Utils.eq(_p33, 'ry') || (_elm_lang$core$Native_Utils.eq(_p33, 'r') || _elm_lang$core$Native_Utils.eq(_p33, 'offset')))))));
			case 'Property4':
				return _elm_lang$core$Native_Utils.eq(_p32._0, 'viewBox');
			default:
				return false;
		}
	}();
};
var _mdgriffith$elm_style_animation$Animation$webkitPrefix = '-webkit-';
var _mdgriffith$elm_style_animation$Animation$iePrefix = '-ms-';
var _mdgriffith$elm_style_animation$Animation$prefix = function (stylePair) {
	var propValue = _elm_lang$core$Tuple$second(stylePair);
	var propName = _elm_lang$core$Tuple$first(stylePair);
	var _p34 = propName;
	switch (_p34) {
		case 'transform':
			return {
				ctor: '::',
				_0: stylePair,
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$iePrefix, propName),
						_1: propValue
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$webkitPrefix, propName),
							_1: propValue
						},
						_1: {ctor: '[]'}
					}
				}
			};
		case 'transform-origin':
			return {
				ctor: '::',
				_0: stylePair,
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$iePrefix, propName),
						_1: propValue
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$webkitPrefix, propName),
							_1: propValue
						},
						_1: {ctor: '[]'}
					}
				}
			};
		case 'filter':
			return {
				ctor: '::',
				_0: stylePair,
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$iePrefix, propName),
						_1: propValue
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: A2(_elm_lang$core$Basics_ops['++'], _mdgriffith$elm_style_animation$Animation$webkitPrefix, propName),
							_1: propValue
						},
						_1: {ctor: '[]'}
					}
				}
			};
		default:
			return {
				ctor: '::',
				_0: stylePair,
				_1: {ctor: '[]'}
			};
	}
};
var _mdgriffith$elm_style_animation$Animation$isFilter = function (prop) {
	return A2(
		_elm_lang$core$List$member,
		_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
		{
			ctor: '::',
			_0: 'filter-url',
			_1: {
				ctor: '::',
				_0: 'blur',
				_1: {
					ctor: '::',
					_0: 'brightness',
					_1: {
						ctor: '::',
						_0: 'contrast',
						_1: {
							ctor: '::',
							_0: 'grayscale',
							_1: {
								ctor: '::',
								_0: 'hue-rotate',
								_1: {
									ctor: '::',
									_0: 'invert',
									_1: {
										ctor: '::',
										_0: 'saturate',
										_1: {
											ctor: '::',
											_0: 'sepia',
											_1: {
												ctor: '::',
												_0: 'drop-shadow',
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$render3dRotation = function (prop) {
	var _p35 = prop;
	if (_p35.ctor === 'Property3') {
		var _p38 = _p35._3;
		var _p37 = _p35._2;
		var _p36 = _p35._1;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'rotateX(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(_p36.position),
				A2(
					_elm_lang$core$Basics_ops['++'],
					_p36.unit,
					A2(
						_elm_lang$core$Basics_ops['++'],
						') rotateY(',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(_p37.position),
							A2(
								_elm_lang$core$Basics_ops['++'],
								_p37.unit,
								A2(
									_elm_lang$core$Basics_ops['++'],
									') rotateZ(',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(_p38.position),
										A2(_elm_lang$core$Basics_ops['++'], _p38.unit, ')')))))))));
	} else {
		return '';
	}
};
var _mdgriffith$elm_style_animation$Animation$isTransformation = function (prop) {
	return A2(
		_elm_lang$core$List$member,
		_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
		{
			ctor: '::',
			_0: 'rotate',
			_1: {
				ctor: '::',
				_0: 'rotateX',
				_1: {
					ctor: '::',
					_0: 'rotateY',
					_1: {
						ctor: '::',
						_0: 'rotateZ',
						_1: {
							ctor: '::',
							_0: 'rotate3d',
							_1: {
								ctor: '::',
								_0: 'translate',
								_1: {
									ctor: '::',
									_0: 'translate3d',
									_1: {
										ctor: '::',
										_0: 'scale',
										_1: {
											ctor: '::',
											_0: 'scale3d',
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$renderAttrs = function (prop) {
	if (A2(
		_elm_lang$core$String$startsWith,
		'attr:',
		_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop))) {
		return _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(
					_elm_lang$core$String$dropLeft,
					5,
					_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop)),
				A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
	} else {
		var _p39 = prop;
		switch (_p39.ctor) {
			case 'Points':
				return _elm_lang$core$Maybe$Just(
					_elm_lang$svg$Svg_Attributes$points(
						A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
			case 'Path':
				return _elm_lang$core$Maybe$Just(
					_elm_lang$svg$Svg_Attributes$d(
						A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
			case 'Property':
				var _p40 = _p39._0;
				switch (_p40) {
					case 'x':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$x(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'y':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$y(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'cx':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$cx(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'cy':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$cy(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'rx':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$rx(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'ry':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$ry(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'r':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$r(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					case 'offset':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$svg$Svg_Attributes$offset(
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')));
					default:
						return _elm_lang$core$Maybe$Nothing;
				}
			case 'Property4':
				return _elm_lang$core$Native_Utils.eq(_p39._0, 'viewBox') ? _elm_lang$core$Maybe$Just(
					_elm_lang$svg$Svg_Attributes$viewBox(
						A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' '))) : _elm_lang$core$Maybe$Nothing;
			default:
				return _elm_lang$core$Maybe$Nothing;
		}
	}
};
var _mdgriffith$elm_style_animation$Animation$render = function (_p41) {
	var _p42 = _p41;
	var _p43 = A2(_elm_lang$core$List$partition, _mdgriffith$elm_style_animation$Animation$isAttr, _p42._0.style);
	var attrProps = _p43._0;
	var styleProps = _p43._1;
	var _p44 = A3(
		_elm_lang$core$List$foldl,
		F2(
			function (prop, _p45) {
				var _p46 = _p45;
				var _p49 = _p46._1;
				var _p48 = _p46._0;
				var _p47 = _p46._2;
				return _mdgriffith$elm_style_animation$Animation$isTransformation(prop) ? {
					ctor: '_Tuple3',
					_0: _p48,
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						_p49,
						{
							ctor: '::',
							_0: prop,
							_1: {ctor: '[]'}
						}),
					_2: _p47
				} : (_mdgriffith$elm_style_animation$Animation$isFilter(prop) ? {
					ctor: '_Tuple3',
					_0: _p48,
					_1: _p49,
					_2: A2(
						_elm_lang$core$Basics_ops['++'],
						_p47,
						{
							ctor: '::',
							_0: prop,
							_1: {ctor: '[]'}
						})
				} : {
					ctor: '_Tuple3',
					_0: A2(
						_elm_lang$core$Basics_ops['++'],
						_p48,
						{
							ctor: '::',
							_0: prop,
							_1: {ctor: '[]'}
						}),
					_1: _p49,
					_2: _p47
				});
			}),
		{
			ctor: '_Tuple3',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'},
			_2: {ctor: '[]'}
		},
		styleProps);
	var style = _p44._0;
	var transforms = _p44._1;
	var filters = _p44._2;
	var renderedStyle = A2(
		_elm_lang$core$List$map,
		function (prop) {
			return {
				ctor: '_Tuple2',
				_0: _mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
				_1: A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ' ')
			};
		},
		style);
	var renderedTransforms = _elm_lang$core$List$isEmpty(transforms) ? {ctor: '[]'} : {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 'transform',
			_1: A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					function (prop) {
						return _elm_lang$core$Native_Utils.eq(
							_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
							'rotate3d') ? _mdgriffith$elm_style_animation$Animation$render3dRotation(prop) : A2(
							_elm_lang$core$Basics_ops['++'],
							_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
							A2(
								_elm_lang$core$Basics_ops['++'],
								'(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ', '),
									')')));
					},
					transforms))
		},
		_1: {ctor: '[]'}
	};
	var renderedFilters = _elm_lang$core$List$isEmpty(filters) ? {ctor: '[]'} : {
		ctor: '::',
		_0: {
			ctor: '_Tuple2',
			_0: 'filter',
			_1: A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					function (prop) {
						var name = _mdgriffith$elm_style_animation$Animation_Model$propertyName(prop);
						return _elm_lang$core$Native_Utils.eq(name, 'filter-url') ? A2(
							_elm_lang$core$Basics_ops['++'],
							'url(\"',
							A2(
								_elm_lang$core$Basics_ops['++'],
								A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ', '),
								'\")')) : A2(
							_elm_lang$core$Basics_ops['++'],
							_mdgriffith$elm_style_animation$Animation_Model$propertyName(prop),
							A2(
								_elm_lang$core$Basics_ops['++'],
								'(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									A2(_mdgriffith$elm_style_animation$Animation$propertyValue, prop, ', '),
									')')));
					},
					filters))
		},
		_1: {ctor: '[]'}
	};
	var styleAttr = _elm_lang$html$Html_Attributes$style(
		A2(
			_elm_lang$core$List$concatMap,
			_mdgriffith$elm_style_animation$Animation$prefix,
			A2(
				_elm_lang$core$Basics_ops['++'],
				renderedTransforms,
				A2(_elm_lang$core$Basics_ops['++'], renderedFilters, renderedStyle))));
	var otherAttrs = A2(_elm_lang$core$List$filterMap, _mdgriffith$elm_style_animation$Animation$renderAttrs, attrProps);
	return {ctor: '::', _0: styleAttr, _1: otherAttrs};
};
var _mdgriffith$elm_style_animation$Animation$alignStartingPoint = function (points) {
	var sums = A2(
		_elm_lang$core$List$map,
		function (_p50) {
			var _p51 = _p50;
			return _p51._0 + _p51._1;
		},
		points);
	var maybeMin = _elm_lang$core$List$minimum(sums);
	var indexOfLowestPoint = function () {
		var _p52 = maybeMin;
		if (_p52.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$List$head(
				A2(
					_elm_lang$core$List$filterMap,
					_elm_lang$core$Basics$identity,
					A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (i, val) {
								return _elm_lang$core$Native_Utils.eq(val, _p52._0) ? _elm_lang$core$Maybe$Just(i) : _elm_lang$core$Maybe$Nothing;
							}),
						sums)));
		}
	}();
	var _p53 = indexOfLowestPoint;
	if (_p53.ctor === 'Nothing') {
		return points;
	} else {
		var _p54 = _p53._0;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_elm_lang$core$List$drop, _p54, points),
			A2(_elm_lang$core$List$take, _p54, points));
	}
};
var _mdgriffith$elm_style_animation$Animation$close = _mdgriffith$elm_style_animation$Animation_Model$Close;
var _mdgriffith$elm_style_animation$Animation$path = function (commands) {
	return _mdgriffith$elm_style_animation$Animation_Model$Path(commands);
};
var _mdgriffith$elm_style_animation$Animation$display = function (mode) {
	return A2(
		_mdgriffith$elm_style_animation$Animation_Model$ExactProperty,
		'display',
		_mdgriffith$elm_style_animation$Animation$displayModeName(mode));
};
var _mdgriffith$elm_style_animation$Animation$exactly = F2(
	function (name, value) {
		return A2(_mdgriffith$elm_style_animation$Animation_Model$ExactProperty, name, value);
	});
var _mdgriffith$elm_style_animation$Animation$filterUrl = function (url) {
	return A2(_mdgriffith$elm_style_animation$Animation$exactly, 'filter-url', url);
};
var _mdgriffith$elm_style_animation$Animation$initMotion = F2(
	function (position, unit) {
		return {
			position: position,
			velocity: 0,
			target: position,
			unit: unit,
			interpolation: _mdgriffith$elm_style_animation$Animation_Model$Spring(
				{stiffness: 170, damping: 26}),
			interpolationOverride: _elm_lang$core$Maybe$Nothing
		};
	});
var _mdgriffith$elm_style_animation$Animation$length = F3(
	function (name, x, unit) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$Property,
			name,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, unit));
	});
var _mdgriffith$elm_style_animation$Animation$strokeWidth = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$length, 'stroke-width', x, '');
};
var _mdgriffith$elm_style_animation$Animation$length2 = F3(
	function (name, _p56, _p55) {
		var _p57 = _p56;
		var _p58 = _p55;
		return A3(
			_mdgriffith$elm_style_animation$Animation_Model$Property2,
			name,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p57._0, _p57._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p58._0, _p58._1));
	});
var _mdgriffith$elm_style_animation$Animation$attr2 = F3(
	function (name, value1, value2) {
		return A3(
			_mdgriffith$elm_style_animation$Animation$length2,
			A2(_elm_lang$core$Basics_ops['++'], 'attr:', name),
			value1,
			value2);
	});
var _mdgriffith$elm_style_animation$Animation$custom2 = F3(
	function (name, value, unit) {
		return A3(_mdgriffith$elm_style_animation$Animation$length2, name, value, unit);
	});
var _mdgriffith$elm_style_animation$Animation$length3 = F4(
	function (name, _p61, _p60, _p59) {
		var _p62 = _p61;
		var _p63 = _p60;
		var _p64 = _p59;
		return A4(
			_mdgriffith$elm_style_animation$Animation_Model$Property3,
			name,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p62._0, _p62._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p63._0, _p63._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p64._0, _p64._1));
	});
var _mdgriffith$elm_style_animation$Animation$attr3 = F4(
	function (name, value1, value2, value3) {
		return A4(
			_mdgriffith$elm_style_animation$Animation$length3,
			A2(_elm_lang$core$Basics_ops['++'], 'attr:', name),
			value1,
			value2,
			value3);
	});
var _mdgriffith$elm_style_animation$Animation$rotate3d = F3(
	function (_p67, _p66, _p65) {
		var _p68 = _p67;
		var _p69 = _p66;
		var _p70 = _p65;
		return A4(
			_mdgriffith$elm_style_animation$Animation$length3,
			'rotate3d',
			{ctor: '_Tuple2', _0: _p68._0, _1: 'rad'},
			{ctor: '_Tuple2', _0: _p69._0, _1: 'rad'},
			{ctor: '_Tuple2', _0: _p70._0, _1: 'rad'});
	});
var _mdgriffith$elm_style_animation$Animation$length4 = F5(
	function (name, _p74, _p73, _p72, _p71) {
		var _p75 = _p74;
		var _p76 = _p73;
		var _p77 = _p72;
		var _p78 = _p71;
		return A5(
			_mdgriffith$elm_style_animation$Animation_Model$Property4,
			name,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p75._0, _p75._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p76._0, _p76._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p77._0, _p77._1),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p78._0, _p78._1));
	});
var _mdgriffith$elm_style_animation$Animation$attr4 = F5(
	function (name, value1, value2, value3, value4) {
		return A5(
			_mdgriffith$elm_style_animation$Animation$length4,
			A2(_elm_lang$core$Basics_ops['++'], 'attr:', name),
			value1,
			value2,
			value3,
			value4);
	});
var _mdgriffith$elm_style_animation$Animation$viewBox = F4(
	function (w, x, y, z) {
		return A5(
			_mdgriffith$elm_style_animation$Animation$length4,
			'viewBox',
			{ctor: '_Tuple2', _0: w, _1: ''},
			{ctor: '_Tuple2', _0: x, _1: ''},
			{ctor: '_Tuple2', _0: y, _1: ''},
			{ctor: '_Tuple2', _0: z, _1: ''});
	});
var _mdgriffith$elm_style_animation$Animation$attr = F3(
	function (name, value, unit) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$Property,
			A2(_elm_lang$core$Basics_ops['++'], 'attr:', name),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, value, unit));
	});
var _mdgriffith$elm_style_animation$Animation$attrColor = F2(
	function (name, color) {
		var _p79 = _elm_lang$core$Color$toRgb(color);
		var red = _p79.red;
		var green = _p79.green;
		var blue = _p79.blue;
		var alpha = _p79.alpha;
		return A5(
			_mdgriffith$elm_style_animation$Animation_Model$ColorProperty,
			A2(_elm_lang$core$Basics_ops['++'], 'attr:', name),
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				''),
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				''),
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, ''));
	});
var _mdgriffith$elm_style_animation$Animation$custom = F3(
	function (name, value, unit) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$Property,
			name,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, value, unit));
	});
var _mdgriffith$elm_style_animation$Animation$opacity = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'opacity', x, '');
};
var _mdgriffith$elm_style_animation$Animation$scale = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'scale', x, '');
};
var _mdgriffith$elm_style_animation$Animation$x = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'x', x, '');
};
var _mdgriffith$elm_style_animation$Animation$y = function (y) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'y', y, '');
};
var _mdgriffith$elm_style_animation$Animation$cx = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'cx', x, '');
};
var _mdgriffith$elm_style_animation$Animation$cy = function (y) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'cy', y, '');
};
var _mdgriffith$elm_style_animation$Animation$radius = function (r) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'r', r, '');
};
var _mdgriffith$elm_style_animation$Animation$radiusX = function (rx) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'rx', rx, '');
};
var _mdgriffith$elm_style_animation$Animation$radiusY = function (ry) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'ry', ry, '');
};
var _mdgriffith$elm_style_animation$Animation$brightness = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'brightness', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$contrast = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'contrast', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$grayscale = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'grayscale', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$greyscale = function (x) {
	return _mdgriffith$elm_style_animation$Animation$grayscale(x);
};
var _mdgriffith$elm_style_animation$Animation$invert = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'invert', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$saturate = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'saturate', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$sepia = function (x) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'sepia', x, '%');
};
var _mdgriffith$elm_style_animation$Animation$offset = function (value) {
	return A3(_mdgriffith$elm_style_animation$Animation$custom, 'offset', value, '');
};
var _mdgriffith$elm_style_animation$Animation$customColor = F2(
	function (name, color) {
		var _p80 = _elm_lang$core$Color$toRgb(color);
		var red = _p80.red;
		var green = _p80.green;
		var blue = _p80.blue;
		var alpha = _p80.alpha;
		return A5(
			_mdgriffith$elm_style_animation$Animation_Model$ColorProperty,
			name,
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				''),
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				''),
			A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, ''));
	});
var _mdgriffith$elm_style_animation$Animation$color = function (c) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'color', c);
};
var _mdgriffith$elm_style_animation$Animation$backgroundColor = function (c) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'background-color', c);
};
var _mdgriffith$elm_style_animation$Animation$borderColor = function (c) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'border-color', c);
};
var _mdgriffith$elm_style_animation$Animation$fill = function (color) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'fill', color);
};
var _mdgriffith$elm_style_animation$Animation$stroke = function (color) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'stroke', color);
};
var _mdgriffith$elm_style_animation$Animation$stopColor = function (color) {
	return A2(_mdgriffith$elm_style_animation$Animation$customColor, 'stop-color', color);
};
var _mdgriffith$elm_style_animation$Animation$scale3d = F3(
	function (x, y, z) {
		return A4(
			_mdgriffith$elm_style_animation$Animation_Model$Property3,
			'scale3d',
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, y, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, z, ''));
	});
var _mdgriffith$elm_style_animation$Animation$rotate = function (_p81) {
	var _p82 = _p81;
	return A2(
		_mdgriffith$elm_style_animation$Animation_Model$AngleProperty,
		'rotate',
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p82._0, 'rad'));
};
var _mdgriffith$elm_style_animation$Animation$textShadow = function (shade) {
	var _p83 = _elm_lang$core$Color$toRgb(shade.color);
	var red = _p83.red;
	var green = _p83.green;
	var blue = _p83.blue;
	var alpha = _p83.alpha;
	return A3(
		_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
		'text-shadow',
		false,
		{
			offsetX: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetX, 'px'),
			offsetY: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetY, 'px'),
			size: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.size, 'px'),
			blur: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.blur, 'px'),
			red: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				'px'),
			green: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				'px'),
			blue: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				'px'),
			alpha: A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, 'px')
		});
};
var _mdgriffith$elm_style_animation$Animation$shadow = function (shade) {
	var _p84 = _elm_lang$core$Color$toRgb(shade.color);
	var red = _p84.red;
	var green = _p84.green;
	var blue = _p84.blue;
	var alpha = _p84.alpha;
	return A3(
		_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
		'box-shadow',
		false,
		{
			offsetX: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetX, 'px'),
			offsetY: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetY, 'px'),
			size: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.size, 'px'),
			blur: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.blur, 'px'),
			red: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				'px'),
			green: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				'px'),
			blue: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				'px'),
			alpha: A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, 'px')
		});
};
var _mdgriffith$elm_style_animation$Animation$insetShadow = function (shade) {
	var _p85 = _elm_lang$core$Color$toRgb(shade.color);
	var red = _p85.red;
	var green = _p85.green;
	var blue = _p85.blue;
	var alpha = _p85.alpha;
	return A3(
		_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
		'box-shadow',
		true,
		{
			offsetX: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetX, 'px'),
			offsetY: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetY, 'px'),
			size: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.size, 'px'),
			blur: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.blur, 'px'),
			red: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				'px'),
			green: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				'px'),
			blue: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				'px'),
			alpha: A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, 'px')
		});
};
var _mdgriffith$elm_style_animation$Animation$move = F2(
	function (x, y) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$Move,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, y, ''));
	});
var _mdgriffith$elm_style_animation$Animation$moveTo = F2(
	function (x, y) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$MoveTo,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, y, ''));
	});
var _mdgriffith$elm_style_animation$Animation$line = F2(
	function (x, y) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$Line,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, y, ''));
	});
var _mdgriffith$elm_style_animation$Animation$lineTo = F2(
	function (x, y) {
		return A2(
			_mdgriffith$elm_style_animation$Animation_Model$LineTo,
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''),
			A2(_mdgriffith$elm_style_animation$Animation$initMotion, y, ''));
	});
var _mdgriffith$elm_style_animation$Animation$horizontal = function (x) {
	return _mdgriffith$elm_style_animation$Animation_Model$Horizontal(
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''));
};
var _mdgriffith$elm_style_animation$Animation$horizontalTo = function (x) {
	return _mdgriffith$elm_style_animation$Animation_Model$HorizontalTo(
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''));
};
var _mdgriffith$elm_style_animation$Animation$vertical = function (x) {
	return _mdgriffith$elm_style_animation$Animation_Model$Vertical(
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''));
};
var _mdgriffith$elm_style_animation$Animation$verticalTo = function (x) {
	return _mdgriffith$elm_style_animation$Animation_Model$VerticalTo(
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, x, ''));
};
var _mdgriffith$elm_style_animation$Animation$curve2 = function (_p86) {
	var _p87 = _p86;
	var _p90 = _p87.point;
	var _p89 = _p87.control2;
	var _p88 = _p87.control1;
	return _mdgriffith$elm_style_animation$Animation_Model$Curve(
		{
			control1: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p88),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p88),
					'')
			},
			control2: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p89),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p89),
					'')
			},
			point: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p90),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p90),
					'')
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$curve2To = function (_p91) {
	var _p92 = _p91;
	var _p95 = _p92.point;
	var _p94 = _p92.control2;
	var _p93 = _p92.control1;
	return _mdgriffith$elm_style_animation$Animation_Model$CurveTo(
		{
			control1: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p93),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p93),
					'')
			},
			control2: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p94),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p94),
					'')
			},
			point: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p95),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p95),
					'')
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$curve = function (_p96) {
	var _p97 = _p96;
	var _p99 = _p97.point;
	var _p98 = _p97.control;
	return _mdgriffith$elm_style_animation$Animation_Model$Quadratic(
		{
			control: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p98),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p98),
					'')
			},
			point: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p99),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p99),
					'')
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$curveTo = function (_p100) {
	var _p101 = _p100;
	var _p103 = _p101.point;
	var _p102 = _p101.control;
	return _mdgriffith$elm_style_animation$Animation_Model$QuadraticTo(
		{
			control: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p102),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p102),
					'')
			},
			point: {
				ctor: '_Tuple2',
				_0: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$first(_p103),
					''),
				_1: A2(
					_mdgriffith$elm_style_animation$Animation$initMotion,
					_elm_lang$core$Tuple$second(_p103),
					'')
			}
		});
};
var _mdgriffith$elm_style_animation$Animation$arc = function (arc) {
	return arc.clockwise ? _mdgriffith$elm_style_animation$Animation_Model$ClockwiseArc(
		{
			x: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.x, ''),
			y: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.y, ''),
			radius: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.radius, ''),
			startAngle: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.startAngle, ''),
			endAngle: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.endAngle, '')
		}) : _mdgriffith$elm_style_animation$Animation_Model$AntiClockwiseArc(
		{
			x: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.x, ''),
			y: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.y, ''),
			radius: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.radius, ''),
			startAngle: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.startAngle, ''),
			endAngle: A2(_mdgriffith$elm_style_animation$Animation$initMotion, arc.endAngle, '')
		});
};
var _mdgriffith$elm_style_animation$Animation$hueRotate = function (_p104) {
	var _p105 = _p104;
	return A2(
		_mdgriffith$elm_style_animation$Animation_Model$AngleProperty,
		'hue-rotate',
		A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p105._0, 'rad'));
};
var _mdgriffith$elm_style_animation$Animation$dropShadow = function (shade) {
	var _p106 = _elm_lang$core$Color$toRgb(shade.color);
	var red = _p106.red;
	var green = _p106.green;
	var blue = _p106.blue;
	var alpha = _p106.alpha;
	return A3(
		_mdgriffith$elm_style_animation$Animation_Model$ShadowProperty,
		'drop-shadow',
		false,
		{
			offsetX: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetX, 'px'),
			offsetY: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.offsetY, 'px'),
			size: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.size, 'px'),
			blur: A2(_mdgriffith$elm_style_animation$Animation$initMotion, shade.blur, 'px'),
			red: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(red),
				'px'),
			green: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(green),
				'px'),
			blue: A2(
				_mdgriffith$elm_style_animation$Animation$initMotion,
				_elm_lang$core$Basics$toFloat(blue),
				'px'),
			alpha: A2(_mdgriffith$elm_style_animation$Animation$initMotion, alpha, 'px')
		});
};
var _mdgriffith$elm_style_animation$Animation$points = function (pnts) {
	return _mdgriffith$elm_style_animation$Animation_Model$Points(
		A2(
			_elm_lang$core$List$map,
			function (_p107) {
				var _p108 = _p107;
				return {
					ctor: '_Tuple2',
					_0: A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p108._0, ''),
					_1: A2(_mdgriffith$elm_style_animation$Animation$initMotion, _p108._1, '')
				};
			},
			_mdgriffith$elm_style_animation$Animation$alignStartingPoint(pnts)));
};
var _mdgriffith$elm_style_animation$Animation$lengthUnitName = function (unit) {
	var _p109 = unit;
	switch (_p109.ctor) {
		case 'NoUnit':
			return '';
		case 'Px':
			return 'px';
		case 'Percent':
			return '%';
		case 'Rem':
			return 'rem';
		case 'Em':
			return 'em';
		case 'Ex':
			return 'ex';
		case 'Ch':
			return 'ch';
		case 'Vh':
			return 'vh';
		case 'Vw':
			return 'vw';
		case 'Vmin':
			return 'vmin';
		case 'Vmax':
			return 'vmax';
		case 'Mm':
			return 'mm';
		case 'Cm':
			return 'cm';
		case 'In':
			return 'in';
		case 'Pt':
			return 'pt';
		default:
			return 'pc';
	}
};
var _mdgriffith$elm_style_animation$Animation$height = function (_p110) {
	var _p111 = _p110;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'height',
		_p111._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p111._1));
};
var _mdgriffith$elm_style_animation$Animation$width = function (_p112) {
	var _p113 = _p112;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'width',
		_p113._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p113._1));
};
var _mdgriffith$elm_style_animation$Animation$left = function (_p114) {
	var _p115 = _p114;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'left',
		_p115._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p115._1));
};
var _mdgriffith$elm_style_animation$Animation$top = function (_p116) {
	var _p117 = _p116;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'top',
		_p117._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p117._1));
};
var _mdgriffith$elm_style_animation$Animation$right = function (_p118) {
	var _p119 = _p118;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'right',
		_p119._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p119._1));
};
var _mdgriffith$elm_style_animation$Animation$bottom = function (_p120) {
	var _p121 = _p120;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'bottom',
		_p121._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p121._1));
};
var _mdgriffith$elm_style_animation$Animation$maxHeight = function (_p122) {
	var _p123 = _p122;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'max-height',
		_p123._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p123._1));
};
var _mdgriffith$elm_style_animation$Animation$maxWidth = function (_p124) {
	var _p125 = _p124;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'max-width',
		_p125._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p125._1));
};
var _mdgriffith$elm_style_animation$Animation$minHeight = function (_p126) {
	var _p127 = _p126;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'min-height',
		_p127._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p127._1));
};
var _mdgriffith$elm_style_animation$Animation$minWidth = function (_p128) {
	var _p129 = _p128;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'min-width',
		_p129._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p129._1));
};
var _mdgriffith$elm_style_animation$Animation$padding = function (_p130) {
	var _p131 = _p130;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'padding',
		_p131._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p131._1));
};
var _mdgriffith$elm_style_animation$Animation$paddingLeft = function (_p132) {
	var _p133 = _p132;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'padding-left',
		_p133._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p133._1));
};
var _mdgriffith$elm_style_animation$Animation$paddingRight = function (_p134) {
	var _p135 = _p134;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'padding-right',
		_p135._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p135._1));
};
var _mdgriffith$elm_style_animation$Animation$paddingTop = function (_p136) {
	var _p137 = _p136;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'padding-top',
		_p137._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p137._1));
};
var _mdgriffith$elm_style_animation$Animation$paddingBottom = function (_p138) {
	var _p139 = _p138;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'padding-bottom',
		_p139._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p139._1));
};
var _mdgriffith$elm_style_animation$Animation$margin = function (_p140) {
	var _p141 = _p140;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'margin',
		_p141._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p141._1));
};
var _mdgriffith$elm_style_animation$Animation$marginLeft = function (_p142) {
	var _p143 = _p142;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'margin-left',
		_p143._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p143._1));
};
var _mdgriffith$elm_style_animation$Animation$marginRight = function (_p144) {
	var _p145 = _p144;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'margin-right',
		_p145._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p145._1));
};
var _mdgriffith$elm_style_animation$Animation$marginTop = function (_p146) {
	var _p147 = _p146;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'margin-top',
		_p147._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p147._1));
};
var _mdgriffith$elm_style_animation$Animation$marginBottom = function (_p148) {
	var _p149 = _p148;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'margin-bottom',
		_p149._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p149._1));
};
var _mdgriffith$elm_style_animation$Animation$borderWidth = function (_p150) {
	var _p151 = _p150;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-width',
		_p151._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p151._1));
};
var _mdgriffith$elm_style_animation$Animation$borderLeftWidth = function (_p152) {
	var _p153 = _p152;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-left-width',
		_p153._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p153._1));
};
var _mdgriffith$elm_style_animation$Animation$borderRightWidth = function (_p154) {
	var _p155 = _p154;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-right-width',
		_p155._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p155._1));
};
var _mdgriffith$elm_style_animation$Animation$borderTopWidth = function (_p156) {
	var _p157 = _p156;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-top-width',
		_p157._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p157._1));
};
var _mdgriffith$elm_style_animation$Animation$borderBottomWidth = function (_p158) {
	var _p159 = _p158;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-bottom-width',
		_p159._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p159._1));
};
var _mdgriffith$elm_style_animation$Animation$borderRadius = function (_p160) {
	var _p161 = _p160;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-radius',
		_p161._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p161._1));
};
var _mdgriffith$elm_style_animation$Animation$borderTopLeftRadius = function (_p162) {
	var _p163 = _p162;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-top-left-radius',
		_p163._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p163._1));
};
var _mdgriffith$elm_style_animation$Animation$borderTopRightRadius = function (_p164) {
	var _p165 = _p164;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-top-right-radius',
		_p165._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p165._1));
};
var _mdgriffith$elm_style_animation$Animation$borderBottomLeftRadius = function (_p166) {
	var _p167 = _p166;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-bottom-left-radius',
		_p167._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p167._1));
};
var _mdgriffith$elm_style_animation$Animation$borderBottomRightRadius = function (_p168) {
	var _p169 = _p168;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'border-bottom-right-radius',
		_p169._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p169._1));
};
var _mdgriffith$elm_style_animation$Animation$letterSpacing = function (_p170) {
	var _p171 = _p170;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'letter-spacing',
		_p171._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p171._1));
};
var _mdgriffith$elm_style_animation$Animation$lineHeight = function (_p172) {
	var _p173 = _p172;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'line-height',
		_p173._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p173._1));
};
var _mdgriffith$elm_style_animation$Animation$backgroundPosition = F2(
	function (_p175, _p174) {
		var _p176 = _p175;
		var _p177 = _p174;
		return A3(
			_mdgriffith$elm_style_animation$Animation$length2,
			'background-position',
			{
				ctor: '_Tuple2',
				_0: _p176._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p176._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p177._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p177._1)
			});
	});
var _mdgriffith$elm_style_animation$Animation$translate = F2(
	function (_p179, _p178) {
		var _p180 = _p179;
		var _p181 = _p178;
		return A3(
			_mdgriffith$elm_style_animation$Animation$length2,
			'translate',
			{
				ctor: '_Tuple2',
				_0: _p180._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p180._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p181._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p181._1)
			});
	});
var _mdgriffith$elm_style_animation$Animation$translate3d = F3(
	function (_p184, _p183, _p182) {
		var _p185 = _p184;
		var _p186 = _p183;
		var _p187 = _p182;
		return A4(
			_mdgriffith$elm_style_animation$Animation$length3,
			'translate3d',
			{
				ctor: '_Tuple2',
				_0: _p185._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p185._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p186._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p186._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p187._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p187._1)
			});
	});
var _mdgriffith$elm_style_animation$Animation$transformOrigin = F3(
	function (_p190, _p189, _p188) {
		var _p191 = _p190;
		var _p192 = _p189;
		var _p193 = _p188;
		return A4(
			_mdgriffith$elm_style_animation$Animation$length3,
			'transform-origin',
			{
				ctor: '_Tuple2',
				_0: _p191._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p191._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p192._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p192._1)
			},
			{
				ctor: '_Tuple2',
				_0: _p193._0,
				_1: _mdgriffith$elm_style_animation$Animation$lengthUnitName(_p193._1)
			});
	});
var _mdgriffith$elm_style_animation$Animation$blur = function (_p194) {
	var _p195 = _p194;
	return A3(
		_mdgriffith$elm_style_animation$Animation$length,
		'blur',
		_p195._0,
		_mdgriffith$elm_style_animation$Animation$lengthUnitName(_p195._1));
};
var _mdgriffith$elm_style_animation$Animation$update = F2(
	function (tick, animation) {
		return _elm_lang$core$Tuple$first(
			A2(_mdgriffith$elm_style_animation$Animation_Model$updateAnimation, tick, animation));
	});
var _mdgriffith$elm_style_animation$Animation$debug = function (_p196) {
	var _p197 = _p196;
	var _p207 = _p197._0;
	var time = _p207.timing.current;
	var getValueTuple = function (prop) {
		var _p198 = prop;
		switch (_p198.ctor) {
			case 'ExactProperty':
				return {ctor: '[]'};
			case 'ColorProperty':
				var _p199 = _p198._0;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$Basics_ops['++'], _p199, '-red'),
						_1: _p198._1,
						_2: time
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$Basics_ops['++'], _p199, '-green'),
							_1: _p198._2,
							_2: time
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple3',
								_0: A2(_elm_lang$core$Basics_ops['++'], _p199, '-blue'),
								_1: _p198._3,
								_2: time
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple3',
									_0: A2(_elm_lang$core$Basics_ops['++'], _p199, '-alpha'),
									_1: _p198._4,
									_2: time
								},
								_1: {ctor: '[]'}
							}
						}
					}
				};
			case 'ShadowProperty':
				var _p201 = _p198._2;
				var _p200 = _p198._0;
				var name = _p198._1 ? A2(_elm_lang$core$Basics_ops['++'], _p200, '-inset') : _p200;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$Basics_ops['++'], name, '-offsetX'),
						_1: _p201.offsetX,
						_2: time
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$Basics_ops['++'], name, '-offsetY'),
							_1: _p201.offsetY,
							_2: time
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple3',
								_0: A2(_elm_lang$core$Basics_ops['++'], name, '-size'),
								_1: _p201.size,
								_2: time
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple3',
									_0: A2(_elm_lang$core$Basics_ops['++'], name, '-blur'),
									_1: _p201.blur,
									_2: time
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple3',
										_0: A2(_elm_lang$core$Basics_ops['++'], name, '-red'),
										_1: _p201.red,
										_2: time
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple3',
											_0: A2(_elm_lang$core$Basics_ops['++'], name, '-green'),
											_1: _p201.green,
											_2: time
										},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple3',
												_0: A2(_elm_lang$core$Basics_ops['++'], name, '-blue'),
												_1: _p201.blue,
												_2: time
											},
											_1: {
												ctor: '::',
												_0: {
													ctor: '_Tuple3',
													_0: A2(_elm_lang$core$Basics_ops['++'], name, '-alpha'),
													_1: _p201.alpha,
													_2: time
												},
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				};
			case 'Property':
				return {
					ctor: '::',
					_0: {ctor: '_Tuple3', _0: _p198._0, _1: _p198._1, _2: time},
					_1: {ctor: '[]'}
				};
			case 'Property2':
				var _p202 = _p198._0;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$Basics_ops['++'], _p202, '-x'),
						_1: _p198._1,
						_2: time
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$Basics_ops['++'], _p202, '-y'),
							_1: _p198._2,
							_2: time
						},
						_1: {ctor: '[]'}
					}
				};
			case 'Property3':
				var _p203 = _p198._0;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$Basics_ops['++'], _p203, '-x'),
						_1: _p198._1,
						_2: time
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$Basics_ops['++'], _p203, '-y'),
							_1: _p198._2,
							_2: time
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple3',
								_0: A2(_elm_lang$core$Basics_ops['++'], _p203, '-z'),
								_1: _p198._3,
								_2: time
							},
							_1: {ctor: '[]'}
						}
					}
				};
			case 'Property4':
				var _p204 = _p198._0;
				return {
					ctor: '::',
					_0: {
						ctor: '_Tuple3',
						_0: A2(_elm_lang$core$Basics_ops['++'], _p204, '-w'),
						_1: _p198._1,
						_2: time
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple3',
							_0: A2(_elm_lang$core$Basics_ops['++'], _p204, '-x'),
							_1: _p198._2,
							_2: time
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple3',
								_0: A2(_elm_lang$core$Basics_ops['++'], _p204, '-y'),
								_1: _p198._3,
								_2: time
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple3',
									_0: A2(_elm_lang$core$Basics_ops['++'], _p204, '-z'),
									_1: _p198._4,
									_2: time
								},
								_1: {ctor: '[]'}
							}
						}
					}
				};
			case 'AngleProperty':
				return {
					ctor: '::',
					_0: {ctor: '_Tuple3', _0: _p198._0, _1: _p198._1, _2: time},
					_1: {ctor: '[]'}
				};
			case 'Points':
				var name = 'points';
				return _elm_lang$core$List$concat(
					A2(
						_elm_lang$core$List$indexedMap,
						F2(
							function (i, _p205) {
								var _p206 = _p205;
								return {
									ctor: '::',
									_0: {
										ctor: '_Tuple3',
										_0: A2(
											_elm_lang$core$Basics_ops['++'],
											_elm_lang$core$Basics$toString(i),
											A2(_elm_lang$core$Basics_ops['++'], name, '-x')),
										_1: _p206._0,
										_2: time
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple3',
											_0: A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(i),
												A2(_elm_lang$core$Basics_ops['++'], name, '-y')),
											_1: _p206._1,
											_2: time
										},
										_1: {ctor: '[]'}
									}
								};
							}),
						_p198._0));
			default:
				return {ctor: '[]'};
		}
	};
	return A2(_elm_lang$core$List$concatMap, getValueTuple, _p207.style);
};
var _mdgriffith$elm_style_animation$Animation$isRunning = function (_p208) {
	var _p209 = _p208;
	return _p209._0.running;
};
var _mdgriffith$elm_style_animation$Animation$subscription = F2(
	function (msg, states) {
		return A2(_elm_lang$core$List$any, _mdgriffith$elm_style_animation$Animation$isRunning, states) ? A2(
			_elm_lang$core$Platform_Sub$map,
			msg,
			_elm_lang$animation_frame$AnimationFrame$times(_mdgriffith$elm_style_animation$Animation_Model$Tick)) : _elm_lang$core$Platform_Sub$none;
	});
var _mdgriffith$elm_style_animation$Animation$extractInitialWait = function (steps) {
	var _p210 = _elm_lang$core$List$head(steps);
	if (_p210.ctor === 'Nothing') {
		return {
			ctor: '_Tuple2',
			_0: 0,
			_1: {ctor: '[]'}
		};
	} else {
		var _p211 = _p210._0;
		if (_p211.ctor === 'Wait') {
			var _p212 = _mdgriffith$elm_style_animation$Animation$extractInitialWait(
				A2(_elm_lang$core$List$drop, 1, steps));
			var additionalTime = _p212._0;
			var remainingSteps = _p212._1;
			return {ctor: '_Tuple2', _0: _p211._0 + additionalTime, _1: remainingSteps};
		} else {
			return {ctor: '_Tuple2', _0: 0, _1: steps};
		}
	}
};
var _mdgriffith$elm_style_animation$Animation$interrupt = F2(
	function (steps, _p213) {
		var _p214 = _p213;
		var _p215 = _p214._0;
		return _mdgriffith$elm_style_animation$Animation_Model$Animation(
			_elm_lang$core$Native_Utils.update(
				_p215,
				{
					interruption: {
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$extractInitialWait(steps),
						_1: _p215.interruption
					},
					running: true
				}));
	});
var _mdgriffith$elm_style_animation$Animation$queue = F2(
	function (steps, _p216) {
		var _p217 = _p216;
		var _p218 = _p217._0;
		return _mdgriffith$elm_style_animation$Animation_Model$Animation(
			_elm_lang$core$Native_Utils.update(
				_p218,
				{
					steps: A2(_elm_lang$core$Basics_ops['++'], _p218.steps, steps),
					running: true
				}));
	});
var _mdgriffith$elm_style_animation$Animation$warnForDoubleListedProperties = function (props) {
	var _p219 = A2(
		_elm_lang$core$List$map,
		function (propGroup) {
			var _p220 = _elm_lang$core$List$head(propGroup);
			if (_p220.ctor === 'Nothing') {
				return '';
			} else {
				return (_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(propGroup),
					1) > 0) ? A2(
					_elm_lang$core$Debug$log,
					'elm-style-animation',
					A2(
						_elm_lang$core$Basics_ops['++'],
						'The \"',
						A2(_elm_lang$core$Basics_ops['++'], _p220._0, '\" css property is listed more than once.  Only the last instance will be used.'))) : '';
			}
		},
		A2(
			_elm_community$list_extra$List_Extra$groupWhile,
			F2(
				function (x, y) {
					return _elm_lang$core$Native_Utils.eq(x, y);
				}),
			_elm_lang$core$List$sort(
				A2(
					_elm_lang$core$List$map,
					_mdgriffith$elm_style_animation$Animation_Model$propertyName,
					A2(
						_elm_lang$core$List$filter,
						function (prop) {
							return !_mdgriffith$elm_style_animation$Animation$isTransformation(prop);
						},
						props)))));
	return props;
};
var _mdgriffith$elm_style_animation$Animation$initialState = function (current) {
	return _mdgriffith$elm_style_animation$Animation_Model$Animation(
		{
			steps: {ctor: '[]'},
			style: current,
			timing: {current: 0, dt: 0},
			running: false,
			interruption: {ctor: '[]'}
		});
};
var _mdgriffith$elm_style_animation$Animation$styleWith = F2(
	function (interp, props) {
		return _mdgriffith$elm_style_animation$Animation$initialState(
			A2(
				_elm_lang$core$List$map,
				_mdgriffith$elm_style_animation$Animation_Model$mapToMotion(
					function (m) {
						return _elm_lang$core$Native_Utils.update(
							m,
							{interpolation: interp});
					}),
				_mdgriffith$elm_style_animation$Animation$warnForDoubleListedProperties(props)));
	});
var _mdgriffith$elm_style_animation$Animation$styleWithEach = function (props) {
	var _p221 = _mdgriffith$elm_style_animation$Animation$warnForDoubleListedProperties(
		A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$second, props));
	return _mdgriffith$elm_style_animation$Animation$initialState(
		A2(
			_elm_lang$core$List$map,
			function (_p222) {
				var _p223 = _p222;
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$mapToMotion,
					function (m) {
						return _elm_lang$core$Native_Utils.update(
							m,
							{interpolation: _p223._0});
					},
					_p223._1);
			},
			props));
};
var _mdgriffith$elm_style_animation$Animation$loop = function (steps) {
	return _mdgriffith$elm_style_animation$Animation_Model$Loop(steps);
};
var _mdgriffith$elm_style_animation$Animation$repeat = F2(
	function (n, steps) {
		return A2(_mdgriffith$elm_style_animation$Animation_Model$Repeat, n, steps);
	});
var _mdgriffith$elm_style_animation$Animation$set = function (props) {
	return _mdgriffith$elm_style_animation$Animation_Model$Set(props);
};
var _mdgriffith$elm_style_animation$Animation$toWithEach = function (interpProps) {
	return _mdgriffith$elm_style_animation$Animation_Model$ToWith(
		A2(
			_elm_lang$core$List$map,
			function (_p224) {
				var _p225 = _p224;
				return A2(
					_mdgriffith$elm_style_animation$Animation_Model$mapToMotion,
					function (m) {
						return _elm_lang$core$Native_Utils.update(
							m,
							{interpolation: _p225._0});
					},
					_p225._1);
			},
			interpProps));
};
var _mdgriffith$elm_style_animation$Animation$toWith = F2(
	function (interp, props) {
		return _mdgriffith$elm_style_animation$Animation_Model$ToWith(
			A2(
				_elm_lang$core$List$map,
				_mdgriffith$elm_style_animation$Animation_Model$mapToMotion(
					function (m) {
						return _elm_lang$core$Native_Utils.update(
							m,
							{interpolation: interp});
					}),
				props));
	});
var _mdgriffith$elm_style_animation$Animation$to = function (props) {
	return _mdgriffith$elm_style_animation$Animation_Model$To(props);
};
var _mdgriffith$elm_style_animation$Animation$wait = function (till) {
	return _mdgriffith$elm_style_animation$Animation_Model$Wait(till);
};
var _mdgriffith$elm_style_animation$Animation$speed = function (speed) {
	return _mdgriffith$elm_style_animation$Animation_Model$AtSpeed(speed);
};
var _mdgriffith$elm_style_animation$Animation$defaultInterpolationByProperty = function (prop) {
	var linear = function (duration) {
		return _mdgriffith$elm_style_animation$Animation_Model$Easing(
			{progress: 1, start: 0, duration: duration, ease: _elm_lang$core$Basics$identity});
	};
	var spring = _mdgriffith$elm_style_animation$Animation_Model$Spring(
		{stiffness: 170, damping: 26});
	var _p226 = prop;
	switch (_p226.ctor) {
		case 'ExactProperty':
			return spring;
		case 'ColorProperty':
			return linear(0.4 * _elm_lang$core$Time$second);
		case 'ShadowProperty':
			return spring;
		case 'Property':
			return spring;
		case 'Property2':
			return spring;
		case 'Property3':
			return _elm_lang$core$Native_Utils.eq(_p226._0, 'rotate3d') ? _mdgriffith$elm_style_animation$Animation$speed(
				{perSecond: _elm_lang$core$Basics$pi}) : spring;
		case 'Property4':
			return spring;
		case 'AngleProperty':
			return _mdgriffith$elm_style_animation$Animation$speed(
				{perSecond: _elm_lang$core$Basics$pi});
		case 'Points':
			return spring;
		default:
			return spring;
	}
};
var _mdgriffith$elm_style_animation$Animation$setDefaultInterpolation = function (prop) {
	var interp = _mdgriffith$elm_style_animation$Animation$defaultInterpolationByProperty(prop);
	return A2(
		_mdgriffith$elm_style_animation$Animation_Model$mapToMotion,
		function (m) {
			return _elm_lang$core$Native_Utils.update(
				m,
				{interpolation: interp});
		},
		prop);
};
var _mdgriffith$elm_style_animation$Animation$style = function (props) {
	return _mdgriffith$elm_style_animation$Animation$initialState(
		A2(
			_elm_lang$core$List$map,
			_mdgriffith$elm_style_animation$Animation$setDefaultInterpolation,
			_mdgriffith$elm_style_animation$Animation$warnForDoubleListedProperties(props)));
};
var _mdgriffith$elm_style_animation$Animation$easing = function (_p227) {
	var _p228 = _p227;
	return _mdgriffith$elm_style_animation$Animation_Model$Easing(
		{progress: 1, duration: _p228.duration, start: 0, ease: _p228.ease});
};
var _mdgriffith$elm_style_animation$Animation$spring = function (settings) {
	return _mdgriffith$elm_style_animation$Animation_Model$Spring(settings);
};
var _mdgriffith$elm_style_animation$Animation$Shadow = F5(
	function (a, b, c, d, e) {
		return {offsetX: a, offsetY: b, size: c, blur: d, color: e};
	});
var _mdgriffith$elm_style_animation$Animation$CubicCurve = F3(
	function (a, b, c) {
		return {control1: a, control2: b, point: c};
	});
var _mdgriffith$elm_style_animation$Animation$QuadraticCurve = F2(
	function (a, b) {
		return {control: a, point: b};
	});
var _mdgriffith$elm_style_animation$Animation$Arc = F6(
	function (a, b, c, d, e, f) {
		return {x: a, y: b, radius: c, startAngle: d, endAngle: e, clockwise: f};
	});
var _mdgriffith$elm_style_animation$Animation$Pc = {ctor: 'Pc'};
var _mdgriffith$elm_style_animation$Animation$Pt = {ctor: 'Pt'};
var _mdgriffith$elm_style_animation$Animation$In = {ctor: 'In'};
var _mdgriffith$elm_style_animation$Animation$Cm = {ctor: 'Cm'};
var _mdgriffith$elm_style_animation$Animation$Mm = {ctor: 'Mm'};
var _mdgriffith$elm_style_animation$Animation$Vmax = {ctor: 'Vmax'};
var _mdgriffith$elm_style_animation$Animation$Vmin = {ctor: 'Vmin'};
var _mdgriffith$elm_style_animation$Animation$Vw = {ctor: 'Vw'};
var _mdgriffith$elm_style_animation$Animation$Vh = {ctor: 'Vh'};
var _mdgriffith$elm_style_animation$Animation$Ch = {ctor: 'Ch'};
var _mdgriffith$elm_style_animation$Animation$Ex = {ctor: 'Ex'};
var _mdgriffith$elm_style_animation$Animation$Em = {ctor: 'Em'};
var _mdgriffith$elm_style_animation$Animation$Rem = {ctor: 'Rem'};
var _mdgriffith$elm_style_animation$Animation$Percent = {ctor: 'Percent'};
var _mdgriffith$elm_style_animation$Animation$Px = {ctor: 'Px'};
var _mdgriffith$elm_style_animation$Animation$NoUnit = {ctor: 'NoUnit'};
var _mdgriffith$elm_style_animation$Animation$Length = F2(
	function (a, b) {
		return {ctor: 'Length', _0: a, _1: b};
	});
var _mdgriffith$elm_style_animation$Animation$px = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Px);
};
var _mdgriffith$elm_style_animation$Animation$percent = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Percent);
};
var _mdgriffith$elm_style_animation$Animation$rem = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Rem);
};
var _mdgriffith$elm_style_animation$Animation$em = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Em);
};
var _mdgriffith$elm_style_animation$Animation$ex = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Ex);
};
var _mdgriffith$elm_style_animation$Animation$ch = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Ch);
};
var _mdgriffith$elm_style_animation$Animation$vh = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Vh);
};
var _mdgriffith$elm_style_animation$Animation$vw = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Vw);
};
var _mdgriffith$elm_style_animation$Animation$vmin = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Vmin);
};
var _mdgriffith$elm_style_animation$Animation$vmax = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Vmax);
};
var _mdgriffith$elm_style_animation$Animation$mm = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Mm);
};
var _mdgriffith$elm_style_animation$Animation$cm = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Cm);
};
var _mdgriffith$elm_style_animation$Animation$inches = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$In);
};
var _mdgriffith$elm_style_animation$Animation$pt = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Pt);
};
var _mdgriffith$elm_style_animation$Animation$pc = function (x) {
	return A2(_mdgriffith$elm_style_animation$Animation$Length, x, _mdgriffith$elm_style_animation$Animation$Pc);
};
var _mdgriffith$elm_style_animation$Animation$Radians = function (a) {
	return {ctor: 'Radians', _0: a};
};
var _mdgriffith$elm_style_animation$Animation$deg = function (a) {
	return _mdgriffith$elm_style_animation$Animation$Radians((a / 360) * (2 * _elm_lang$core$Basics$pi));
};
var _mdgriffith$elm_style_animation$Animation$grad = function (a) {
	return _mdgriffith$elm_style_animation$Animation$Radians((a / 400) * (2 * _elm_lang$core$Basics$pi));
};
var _mdgriffith$elm_style_animation$Animation$rad = function (a) {
	return _mdgriffith$elm_style_animation$Animation$Radians(a);
};
var _mdgriffith$elm_style_animation$Animation$turn = function (a) {
	return _mdgriffith$elm_style_animation$Animation$Radians(a * (2 * _elm_lang$core$Basics$pi));
};
var _mdgriffith$elm_style_animation$Animation$ListItem = {ctor: 'ListItem'};
var _mdgriffith$elm_style_animation$Animation$listItem = _mdgriffith$elm_style_animation$Animation$ListItem;
var _mdgriffith$elm_style_animation$Animation$InlineFlex = {ctor: 'InlineFlex'};
var _mdgriffith$elm_style_animation$Animation$inlineFlex = _mdgriffith$elm_style_animation$Animation$InlineFlex;
var _mdgriffith$elm_style_animation$Animation$Flex = {ctor: 'Flex'};
var _mdgriffith$elm_style_animation$Animation$flex = _mdgriffith$elm_style_animation$Animation$Flex;
var _mdgriffith$elm_style_animation$Animation$Block = {ctor: 'Block'};
var _mdgriffith$elm_style_animation$Animation$block = _mdgriffith$elm_style_animation$Animation$Block;
var _mdgriffith$elm_style_animation$Animation$InlineBlock = {ctor: 'InlineBlock'};
var _mdgriffith$elm_style_animation$Animation$inlineBlock = _mdgriffith$elm_style_animation$Animation$InlineBlock;
var _mdgriffith$elm_style_animation$Animation$Inline = {ctor: 'Inline'};
var _mdgriffith$elm_style_animation$Animation$inline = _mdgriffith$elm_style_animation$Animation$Inline;
var _mdgriffith$elm_style_animation$Animation$None = {ctor: 'None'};
var _mdgriffith$elm_style_animation$Animation$none = _mdgriffith$elm_style_animation$Animation$None;

var _mdgriffith$elm_style_animation$Animation_Messenger$send = function (msg) {
	return _mdgriffith$elm_style_animation$Animation_Model$Send(msg);
};
var _mdgriffith$elm_style_animation$Animation_Messenger$update = F2(
	function (tick, animation) {
		return A2(_mdgriffith$elm_style_animation$Animation_Model$updateAnimation, tick, animation);
	});

var _ohanhi$keyboard_extra$Keyboard_Arrows$boolToInt = function (bool) {
	return bool ? 1 : 0;
};
var _ohanhi$keyboard_extra$Keyboard_Arrows$determineArrows = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Arrows$boolToInt(
			A2(_elm_lang$core$Set$member, key, keys));
	};
	var x = toInt(39) - toInt(37);
	var y = toInt(38) - toInt(40);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Arrows$determineWasd = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Arrows$boolToInt(
			A2(_elm_lang$core$Set$member, key, keys));
	};
	var x = toInt(68) - toInt(65);
	var y = toInt(87) - toInt(83);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Arrows$init = {x: 0, y: 0};
var _ohanhi$keyboard_extra$Keyboard_Arrows$Arrows = F2(
	function (a, b) {
		return {x: a, y: b};
	});

var _ohanhi$keyboard_extra$Keyboard_Extra$wasd = function (model) {
	return _ohanhi$keyboard_extra$Keyboard_Arrows$determineWasd(model.keysDown);
};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrows = function (model) {
	return _ohanhi$keyboard_extra$Keyboard_Arrows$determineArrows(model.keysDown);
};
var _ohanhi$keyboard_extra$Keyboard_Extra$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'Down') {
			var keysDown = A2(_elm_lang$core$Set$insert, _p0._0, model.keysDown);
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{keysDown: keysDown}),
				{ctor: '[]'});
		} else {
			var keysDown = A2(_elm_lang$core$Set$remove, _p0._0, model.keysDown);
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{keysDown: keysDown}),
				{ctor: '[]'});
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$Model = function (a) {
	return {keysDown: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$init = {
	ctor: '_Tuple2',
	_0: _ohanhi$keyboard_extra$Keyboard_Extra$Model(_elm_lang$core$Set$empty),
	_1: _elm_lang$core$Platform_Cmd$none
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Up = function (a) {
	return {ctor: 'Up', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$subscriptions = _elm_lang$core$Platform_Sub$batch(
	{
		ctor: '::',
		_0: _elm_lang$keyboard$Keyboard$downs(_ohanhi$keyboard_extra$Keyboard_Extra$Down),
		_1: {
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$ups(_ohanhi$keyboard_extra$Keyboard_Extra$Up),
			_1: {ctor: '[]'}
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection = {ctor: 'NoDirection'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest = {ctor: 'NorthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$West = {ctor: 'West'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest = {ctor: 'SouthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$South = {ctor: 'South'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast = {ctor: 'SouthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$East = {ctor: 'East'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast = {ctor: 'NorthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$North = {ctor: 'North'};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir = function (_p1) {
	var _p2 = _p1;
	var _p3 = {ctor: '_Tuple2', _0: _p2.x, _1: _p2.y};
	_v2_8:
	do {
		if (_p3.ctor === '_Tuple2') {
			switch (_p3._0) {
				case 1:
					switch (_p3._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$East;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast;
						default:
							break _v2_8;
					}
				case 0:
					switch (_p3._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$North;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$South;
						default:
							break _v2_8;
					}
				case -1:
					switch (_p3._1) {
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$West;
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest;
						default:
							break _v2_8;
					}
				default:
					break _v2_8;
			}
		} else {
			break _v2_8;
		}
	} while(false);
	return _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection;
};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsDirection = function (_p4) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$arrows(_p4));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$wasdDirection = function (_p5) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$wasd(_p5));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Other = {ctor: 'Other'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Altgr = {ctor: 'Altgr'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Meta = {ctor: 'Meta'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Quote = {ctor: 'Quote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket = {ctor: 'CloseBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash = {ctor: 'BackSlash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket = {ctor: 'OpenBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote = {ctor: 'BackQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Slash = {ctor: 'Slash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Period = {ctor: 'Period'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Minus = {ctor: 'Minus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Comma = {ctor: 'Comma'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp = {ctor: 'VolumeUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown = {ctor: 'VolumeDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute = {ctor: 'VolumeMute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tilde = {ctor: 'Tilde'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket = {ctor: 'CloseCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket = {ctor: 'OpenCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus = {ctor: 'HyphenMinus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pipe = {ctor: 'Pipe'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Plus = {ctor: 'Plus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk = {ctor: 'Asterisk'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen = {ctor: 'CloseParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen = {ctor: 'OpenParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Underscore = {ctor: 'Underscore'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand = {ctor: 'Ampersand'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Percent = {ctor: 'Percent'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Dollar = {ctor: 'Dollar'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Hash = {ctor: 'Hash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote = {ctor: 'DoubleQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation = {ctor: 'Exclamation'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex = {ctor: 'Circumflex'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock = {ctor: 'ScrollLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NumLock = {ctor: 'NumLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F24 = {ctor: 'F24'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F23 = {ctor: 'F23'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F22 = {ctor: 'F22'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F21 = {ctor: 'F21'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F20 = {ctor: 'F20'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F19 = {ctor: 'F19'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F18 = {ctor: 'F18'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F17 = {ctor: 'F17'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F16 = {ctor: 'F16'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F15 = {ctor: 'F15'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F14 = {ctor: 'F14'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F13 = {ctor: 'F13'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F12 = {ctor: 'F12'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F11 = {ctor: 'F11'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F10 = {ctor: 'F10'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F9 = {ctor: 'F9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F8 = {ctor: 'F8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F7 = {ctor: 'F7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F6 = {ctor: 'F6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F5 = {ctor: 'F5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F4 = {ctor: 'F4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F3 = {ctor: 'F3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F2 = {ctor: 'F2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F1 = {ctor: 'F1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Divide = {ctor: 'Divide'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Decimal = {ctor: 'Decimal'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Subtract = {ctor: 'Subtract'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Separator = {ctor: 'Separator'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Add = {ctor: 'Add'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Multiply = {ctor: 'Multiply'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9 = {ctor: 'Numpad9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8 = {ctor: 'Numpad8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7 = {ctor: 'Numpad7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6 = {ctor: 'Numpad6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5 = {ctor: 'Numpad5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4 = {ctor: 'Numpad4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3 = {ctor: 'Numpad3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2 = {ctor: 'Numpad2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1 = {ctor: 'Numpad1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0 = {ctor: 'Numpad0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Sleep = {ctor: 'Sleep'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu = {ctor: 'ContextMenu'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Super = {ctor: 'Super'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharZ = {ctor: 'CharZ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharY = {ctor: 'CharY'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharX = {ctor: 'CharX'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharW = {ctor: 'CharW'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharV = {ctor: 'CharV'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharU = {ctor: 'CharU'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharT = {ctor: 'CharT'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharS = {ctor: 'CharS'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharR = {ctor: 'CharR'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharQ = {ctor: 'CharQ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharP = {ctor: 'CharP'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharO = {ctor: 'CharO'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharN = {ctor: 'CharN'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharM = {ctor: 'CharM'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharL = {ctor: 'CharL'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharK = {ctor: 'CharK'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharJ = {ctor: 'CharJ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharI = {ctor: 'CharI'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharH = {ctor: 'CharH'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharG = {ctor: 'CharG'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharF = {ctor: 'CharF'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharE = {ctor: 'CharE'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharD = {ctor: 'CharD'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharC = {ctor: 'CharC'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharB = {ctor: 'CharB'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharA = {ctor: 'CharA'};
var _ohanhi$keyboard_extra$Keyboard_Extra$At = {ctor: 'At'};
var _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark = {ctor: 'QuestionMark'};
var _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan = {ctor: 'GreaterThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Equals = {ctor: 'Equals'};
var _ohanhi$keyboard_extra$Keyboard_Extra$LessThan = {ctor: 'LessThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon = {ctor: 'Semicolon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Colon = {ctor: 'Colon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number9 = {ctor: 'Number9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number8 = {ctor: 'Number8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number7 = {ctor: 'Number7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number6 = {ctor: 'Number6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number5 = {ctor: 'Number5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number4 = {ctor: 'Number4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number3 = {ctor: 'Number3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number2 = {ctor: 'Number2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number1 = {ctor: 'Number1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number0 = {ctor: 'Number0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Delete = {ctor: 'Delete'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Insert = {ctor: 'Insert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen = {ctor: 'PrintScreen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Execute = {ctor: 'Execute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Print = {ctor: 'Print'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Select = {ctor: 'Select'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown = {ctor: 'ArrowDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight = {ctor: 'ArrowRight'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp = {ctor: 'ArrowUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft = {ctor: 'ArrowLeft'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Home = {ctor: 'Home'};
var _ohanhi$keyboard_extra$Keyboard_Extra$End = {ctor: 'End'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageDown = {ctor: 'PageDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageUp = {ctor: 'PageUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Space = {ctor: 'Space'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange = {ctor: 'ModeChange'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Accept = {ctor: 'Accept'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert = {ctor: 'NonConvert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Convert = {ctor: 'Convert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Escape = {ctor: 'Escape'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock = {ctor: 'CapsLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pause = {ctor: 'Pause'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Alt = {ctor: 'Alt'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Control = {ctor: 'Control'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Shift = {ctor: 'Shift'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Enter = {ctor: 'Enter'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Clear = {ctor: 'Clear'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tab = {ctor: 'Tab'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace = {ctor: 'BackSpace'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Help = {ctor: 'Help'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Cancel = {ctor: 'Cancel'};
var _ohanhi$keyboard_extra$Keyboard_Extra$codeBook = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 3, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Cancel},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 6, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Help},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 8, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 9, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tab},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 12, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Clear},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 13, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Enter},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 16, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Shift},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 17, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Control},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 18, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Alt},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 19, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pause},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 20, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 27, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Escape},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 28, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Convert},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 29, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 30, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Accept},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 31, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 32, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Space},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 33, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageUp},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 34, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageDown},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 35, _1: _ohanhi$keyboard_extra$Keyboard_Extra$End},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 36, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Home},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 37, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 38, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 39, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 40, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 41, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Select},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 42, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Print},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 43, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Execute},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 44, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 45, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Insert},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 46, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Delete},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 48, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number0},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 49, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number1},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 50, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number2},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 51, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number3},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 52, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number4},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 53, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number5},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 54, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number6},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 55, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number7},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 56, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number8},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 57, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number9},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 58, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Colon},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 59, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 60, _1: _ohanhi$keyboard_extra$Keyboard_Extra$LessThan},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 61, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 62, _1: _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 63, _1: _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 64, _1: _ohanhi$keyboard_extra$Keyboard_Extra$At},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 65, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharA},
																																																	_1: {
																																																		ctor: '::',
																																																		_0: {ctor: '_Tuple2', _0: 66, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharB},
																																																		_1: {
																																																			ctor: '::',
																																																			_0: {ctor: '_Tuple2', _0: 67, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharC},
																																																			_1: {
																																																				ctor: '::',
																																																				_0: {ctor: '_Tuple2', _0: 68, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharD},
																																																				_1: {
																																																					ctor: '::',
																																																					_0: {ctor: '_Tuple2', _0: 69, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharE},
																																																					_1: {
																																																						ctor: '::',
																																																						_0: {ctor: '_Tuple2', _0: 70, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharF},
																																																						_1: {
																																																							ctor: '::',
																																																							_0: {ctor: '_Tuple2', _0: 71, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharG},
																																																							_1: {
																																																								ctor: '::',
																																																								_0: {ctor: '_Tuple2', _0: 72, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharH},
																																																								_1: {
																																																									ctor: '::',
																																																									_0: {ctor: '_Tuple2', _0: 73, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharI},
																																																									_1: {
																																																										ctor: '::',
																																																										_0: {ctor: '_Tuple2', _0: 74, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharJ},
																																																										_1: {
																																																											ctor: '::',
																																																											_0: {ctor: '_Tuple2', _0: 75, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharK},
																																																											_1: {
																																																												ctor: '::',
																																																												_0: {ctor: '_Tuple2', _0: 76, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharL},
																																																												_1: {
																																																													ctor: '::',
																																																													_0: {ctor: '_Tuple2', _0: 77, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharM},
																																																													_1: {
																																																														ctor: '::',
																																																														_0: {ctor: '_Tuple2', _0: 78, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharN},
																																																														_1: {
																																																															ctor: '::',
																																																															_0: {ctor: '_Tuple2', _0: 79, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharO},
																																																															_1: {
																																																																ctor: '::',
																																																																_0: {ctor: '_Tuple2', _0: 80, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharP},
																																																																_1: {
																																																																	ctor: '::',
																																																																	_0: {ctor: '_Tuple2', _0: 81, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharQ},
																																																																	_1: {
																																																																		ctor: '::',
																																																																		_0: {ctor: '_Tuple2', _0: 82, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharR},
																																																																		_1: {
																																																																			ctor: '::',
																																																																			_0: {ctor: '_Tuple2', _0: 83, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharS},
																																																																			_1: {
																																																																				ctor: '::',
																																																																				_0: {ctor: '_Tuple2', _0: 84, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharT},
																																																																				_1: {
																																																																					ctor: '::',
																																																																					_0: {ctor: '_Tuple2', _0: 85, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharU},
																																																																					_1: {
																																																																						ctor: '::',
																																																																						_0: {ctor: '_Tuple2', _0: 86, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharV},
																																																																						_1: {
																																																																							ctor: '::',
																																																																							_0: {ctor: '_Tuple2', _0: 87, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharW},
																																																																							_1: {
																																																																								ctor: '::',
																																																																								_0: {ctor: '_Tuple2', _0: 88, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharX},
																																																																								_1: {
																																																																									ctor: '::',
																																																																									_0: {ctor: '_Tuple2', _0: 89, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharY},
																																																																									_1: {
																																																																										ctor: '::',
																																																																										_0: {ctor: '_Tuple2', _0: 90, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharZ},
																																																																										_1: {
																																																																											ctor: '::',
																																																																											_0: {ctor: '_Tuple2', _0: 91, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Super},
																																																																											_1: {
																																																																												ctor: '::',
																																																																												_0: {ctor: '_Tuple2', _0: 93, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu},
																																																																												_1: {
																																																																													ctor: '::',
																																																																													_0: {ctor: '_Tuple2', _0: 95, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Sleep},
																																																																													_1: {
																																																																														ctor: '::',
																																																																														_0: {ctor: '_Tuple2', _0: 96, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0},
																																																																														_1: {
																																																																															ctor: '::',
																																																																															_0: {ctor: '_Tuple2', _0: 97, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1},
																																																																															_1: {
																																																																																ctor: '::',
																																																																																_0: {ctor: '_Tuple2', _0: 98, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2},
																																																																																_1: {
																																																																																	ctor: '::',
																																																																																	_0: {ctor: '_Tuple2', _0: 99, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3},
																																																																																	_1: {
																																																																																		ctor: '::',
																																																																																		_0: {ctor: '_Tuple2', _0: 100, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4},
																																																																																		_1: {
																																																																																			ctor: '::',
																																																																																			_0: {ctor: '_Tuple2', _0: 101, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5},
																																																																																			_1: {
																																																																																				ctor: '::',
																																																																																				_0: {ctor: '_Tuple2', _0: 102, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6},
																																																																																				_1: {
																																																																																					ctor: '::',
																																																																																					_0: {ctor: '_Tuple2', _0: 103, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7},
																																																																																					_1: {
																																																																																						ctor: '::',
																																																																																						_0: {ctor: '_Tuple2', _0: 104, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8},
																																																																																						_1: {
																																																																																							ctor: '::',
																																																																																							_0: {ctor: '_Tuple2', _0: 105, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9},
																																																																																							_1: {
																																																																																								ctor: '::',
																																																																																								_0: {ctor: '_Tuple2', _0: 106, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Multiply},
																																																																																								_1: {
																																																																																									ctor: '::',
																																																																																									_0: {ctor: '_Tuple2', _0: 107, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Add},
																																																																																									_1: {
																																																																																										ctor: '::',
																																																																																										_0: {ctor: '_Tuple2', _0: 108, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Separator},
																																																																																										_1: {
																																																																																											ctor: '::',
																																																																																											_0: {ctor: '_Tuple2', _0: 109, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Subtract},
																																																																																											_1: {
																																																																																												ctor: '::',
																																																																																												_0: {ctor: '_Tuple2', _0: 110, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Decimal},
																																																																																												_1: {
																																																																																													ctor: '::',
																																																																																													_0: {ctor: '_Tuple2', _0: 111, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Divide},
																																																																																													_1: {
																																																																																														ctor: '::',
																																																																																														_0: {ctor: '_Tuple2', _0: 112, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F1},
																																																																																														_1: {
																																																																																															ctor: '::',
																																																																																															_0: {ctor: '_Tuple2', _0: 113, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F2},
																																																																																															_1: {
																																																																																																ctor: '::',
																																																																																																_0: {ctor: '_Tuple2', _0: 114, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F3},
																																																																																																_1: {
																																																																																																	ctor: '::',
																																																																																																	_0: {ctor: '_Tuple2', _0: 115, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F4},
																																																																																																	_1: {
																																																																																																		ctor: '::',
																																																																																																		_0: {ctor: '_Tuple2', _0: 116, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F5},
																																																																																																		_1: {
																																																																																																			ctor: '::',
																																																																																																			_0: {ctor: '_Tuple2', _0: 117, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F6},
																																																																																																			_1: {
																																																																																																				ctor: '::',
																																																																																																				_0: {ctor: '_Tuple2', _0: 118, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F7},
																																																																																																				_1: {
																																																																																																					ctor: '::',
																																																																																																					_0: {ctor: '_Tuple2', _0: 119, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F8},
																																																																																																					_1: {
																																																																																																						ctor: '::',
																																																																																																						_0: {ctor: '_Tuple2', _0: 120, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F9},
																																																																																																						_1: {
																																																																																																							ctor: '::',
																																																																																																							_0: {ctor: '_Tuple2', _0: 121, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F10},
																																																																																																							_1: {
																																																																																																								ctor: '::',
																																																																																																								_0: {ctor: '_Tuple2', _0: 122, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F11},
																																																																																																								_1: {
																																																																																																									ctor: '::',
																																																																																																									_0: {ctor: '_Tuple2', _0: 123, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F12},
																																																																																																									_1: {
																																																																																																										ctor: '::',
																																																																																																										_0: {ctor: '_Tuple2', _0: 124, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F13},
																																																																																																										_1: {
																																																																																																											ctor: '::',
																																																																																																											_0: {ctor: '_Tuple2', _0: 125, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F14},
																																																																																																											_1: {
																																																																																																												ctor: '::',
																																																																																																												_0: {ctor: '_Tuple2', _0: 126, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F15},
																																																																																																												_1: {
																																																																																																													ctor: '::',
																																																																																																													_0: {ctor: '_Tuple2', _0: 127, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F16},
																																																																																																													_1: {
																																																																																																														ctor: '::',
																																																																																																														_0: {ctor: '_Tuple2', _0: 128, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F17},
																																																																																																														_1: {
																																																																																																															ctor: '::',
																																																																																																															_0: {ctor: '_Tuple2', _0: 129, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F18},
																																																																																																															_1: {
																																																																																																																ctor: '::',
																																																																																																																_0: {ctor: '_Tuple2', _0: 130, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F19},
																																																																																																																_1: {
																																																																																																																	ctor: '::',
																																																																																																																	_0: {ctor: '_Tuple2', _0: 131, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F20},
																																																																																																																	_1: {
																																																																																																																		ctor: '::',
																																																																																																																		_0: {ctor: '_Tuple2', _0: 132, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F21},
																																																																																																																		_1: {
																																																																																																																			ctor: '::',
																																																																																																																			_0: {ctor: '_Tuple2', _0: 133, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F22},
																																																																																																																			_1: {
																																																																																																																				ctor: '::',
																																																																																																																				_0: {ctor: '_Tuple2', _0: 134, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F23},
																																																																																																																				_1: {
																																																																																																																					ctor: '::',
																																																																																																																					_0: {ctor: '_Tuple2', _0: 135, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F24},
																																																																																																																					_1: {
																																																																																																																						ctor: '::',
																																																																																																																						_0: {ctor: '_Tuple2', _0: 144, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NumLock},
																																																																																																																						_1: {
																																																																																																																							ctor: '::',
																																																																																																																							_0: {ctor: '_Tuple2', _0: 145, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock},
																																																																																																																							_1: {
																																																																																																																								ctor: '::',
																																																																																																																								_0: {ctor: '_Tuple2', _0: 160, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex},
																																																																																																																								_1: {
																																																																																																																									ctor: '::',
																																																																																																																									_0: {ctor: '_Tuple2', _0: 161, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation},
																																																																																																																									_1: {
																																																																																																																										ctor: '::',
																																																																																																																										_0: {ctor: '_Tuple2', _0: 162, _1: _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote},
																																																																																																																										_1: {
																																																																																																																											ctor: '::',
																																																																																																																											_0: {ctor: '_Tuple2', _0: 163, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Hash},
																																																																																																																											_1: {
																																																																																																																												ctor: '::',
																																																																																																																												_0: {ctor: '_Tuple2', _0: 164, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Dollar},
																																																																																																																												_1: {
																																																																																																																													ctor: '::',
																																																																																																																													_0: {ctor: '_Tuple2', _0: 165, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Percent},
																																																																																																																													_1: {
																																																																																																																														ctor: '::',
																																																																																																																														_0: {ctor: '_Tuple2', _0: 166, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand},
																																																																																																																														_1: {
																																																																																																																															ctor: '::',
																																																																																																																															_0: {ctor: '_Tuple2', _0: 167, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Underscore},
																																																																																																																															_1: {
																																																																																																																																ctor: '::',
																																																																																																																																_0: {ctor: '_Tuple2', _0: 168, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen},
																																																																																																																																_1: {
																																																																																																																																	ctor: '::',
																																																																																																																																	_0: {ctor: '_Tuple2', _0: 169, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen},
																																																																																																																																	_1: {
																																																																																																																																		ctor: '::',
																																																																																																																																		_0: {ctor: '_Tuple2', _0: 170, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk},
																																																																																																																																		_1: {
																																																																																																																																			ctor: '::',
																																																																																																																																			_0: {ctor: '_Tuple2', _0: 171, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Plus},
																																																																																																																																			_1: {
																																																																																																																																				ctor: '::',
																																																																																																																																				_0: {ctor: '_Tuple2', _0: 172, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pipe},
																																																																																																																																				_1: {
																																																																																																																																					ctor: '::',
																																																																																																																																					_0: {ctor: '_Tuple2', _0: 173, _1: _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus},
																																																																																																																																					_1: {
																																																																																																																																						ctor: '::',
																																																																																																																																						_0: {ctor: '_Tuple2', _0: 174, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket},
																																																																																																																																						_1: {
																																																																																																																																							ctor: '::',
																																																																																																																																							_0: {ctor: '_Tuple2', _0: 175, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket},
																																																																																																																																							_1: {
																																																																																																																																								ctor: '::',
																																																																																																																																								_0: {ctor: '_Tuple2', _0: 176, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tilde},
																																																																																																																																								_1: {
																																																																																																																																									ctor: '::',
																																																																																																																																									_0: {ctor: '_Tuple2', _0: 181, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute},
																																																																																																																																									_1: {
																																																																																																																																										ctor: '::',
																																																																																																																																										_0: {ctor: '_Tuple2', _0: 182, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown},
																																																																																																																																										_1: {
																																																																																																																																											ctor: '::',
																																																																																																																																											_0: {ctor: '_Tuple2', _0: 183, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp},
																																																																																																																																											_1: {
																																																																																																																																												ctor: '::',
																																																																																																																																												_0: {ctor: '_Tuple2', _0: 186, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																																																																																																																												_1: {
																																																																																																																																													ctor: '::',
																																																																																																																																													_0: {ctor: '_Tuple2', _0: 187, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																																																																																																																													_1: {
																																																																																																																																														ctor: '::',
																																																																																																																																														_0: {ctor: '_Tuple2', _0: 188, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Comma},
																																																																																																																																														_1: {
																																																																																																																																															ctor: '::',
																																																																																																																																															_0: {ctor: '_Tuple2', _0: 189, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Minus},
																																																																																																																																															_1: {
																																																																																																																																																ctor: '::',
																																																																																																																																																_0: {ctor: '_Tuple2', _0: 190, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Period},
																																																																																																																																																_1: {
																																																																																																																																																	ctor: '::',
																																																																																																																																																	_0: {ctor: '_Tuple2', _0: 191, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Slash},
																																																																																																																																																	_1: {
																																																																																																																																																		ctor: '::',
																																																																																																																																																		_0: {ctor: '_Tuple2', _0: 192, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote},
																																																																																																																																																		_1: {
																																																																																																																																																			ctor: '::',
																																																																																																																																																			_0: {ctor: '_Tuple2', _0: 219, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket},
																																																																																																																																																			_1: {
																																																																																																																																																				ctor: '::',
																																																																																																																																																				_0: {ctor: '_Tuple2', _0: 220, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash},
																																																																																																																																																				_1: {
																																																																																																																																																					ctor: '::',
																																																																																																																																																					_0: {ctor: '_Tuple2', _0: 221, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket},
																																																																																																																																																					_1: {
																																																																																																																																																						ctor: '::',
																																																																																																																																																						_0: {ctor: '_Tuple2', _0: 222, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Quote},
																																																																																																																																																						_1: {
																																																																																																																																																							ctor: '::',
																																																																																																																																																							_0: {ctor: '_Tuple2', _0: 224, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Meta},
																																																																																																																																																							_1: {
																																																																																																																																																								ctor: '::',
																																																																																																																																																								_0: {ctor: '_Tuple2', _0: 225, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Altgr},
																																																																																																																																																								_1: {ctor: '[]'}
																																																																																																																																																							}
																																																																																																																																																						}
																																																																																																																																																					}
																																																																																																																																																				}
																																																																																																																																																			}
																																																																																																																																																		}
																																																																																																																																																	}
																																																																																																																																																}
																																																																																																																																															}
																																																																																																																																														}
																																																																																																																																													}
																																																																																																																																												}
																																																																																																																																											}
																																																																																																																																										}
																																																																																																																																									}
																																																																																																																																								}
																																																																																																																																							}
																																																																																																																																						}
																																																																																																																																					}
																																																																																																																																				}
																																																																																																																																			}
																																																																																																																																		}
																																																																																																																																	}
																																																																																																																																}
																																																																																																																															}
																																																																																																																														}
																																																																																																																													}
																																																																																																																												}
																																																																																																																											}
																																																																																																																										}
																																																																																																																									}
																																																																																																																								}
																																																																																																																							}
																																																																																																																						}
																																																																																																																					}
																																																																																																																				}
																																																																																																																			}
																																																																																																																		}
																																																																																																																	}
																																																																																																																}
																																																																																																															}
																																																																																																														}
																																																																																																													}
																																																																																																												}
																																																																																																											}
																																																																																																										}
																																																																																																									}
																																																																																																								}
																																																																																																							}
																																																																																																						}
																																																																																																					}
																																																																																																				}
																																																																																																			}
																																																																																																		}
																																																																																																	}
																																																																																																}
																																																																																															}
																																																																																														}
																																																																																													}
																																																																																												}
																																																																																											}
																																																																																										}
																																																																																									}
																																																																																								}
																																																																																							}
																																																																																						}
																																																																																					}
																																																																																				}
																																																																																			}
																																																																																		}
																																																																																	}
																																																																																}
																																																																															}
																																																																														}
																																																																													}
																																																																												}
																																																																											}
																																																																										}
																																																																									}
																																																																								}
																																																																							}
																																																																						}
																																																																					}
																																																																				}
																																																																			}
																																																																		}
																																																																	}
																																																																}
																																																															}
																																																														}
																																																													}
																																																												}
																																																											}
																																																										}
																																																									}
																																																								}
																																																							}
																																																						}
																																																					}
																																																				}
																																																			}
																																																		}
																																																	}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _ohanhi$keyboard_extra$Keyboard_Extra$toCode = function (key) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(
					_elm_lang$core$List$filter,
					function (_p6) {
						return A2(
							F2(
								function (x, y) {
									return _elm_lang$core$Native_Utils.eq(x, y);
								}),
							key,
							_elm_lang$core$Tuple$second(_p6));
					},
					_ohanhi$keyboard_extra$Keyboard_Extra$codeBook))));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$isPressed = F2(
	function (key, model) {
		return A2(
			_elm_lang$core$Set$member,
			_ohanhi$keyboard_extra$Keyboard_Extra$toCode(key),
			model.keysDown);
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$codeDict = _elm_lang$core$Dict$fromList(_ohanhi$keyboard_extra$Keyboard_Extra$codeBook);
var _ohanhi$keyboard_extra$Keyboard_Extra$fromCode = function (code) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_ohanhi$keyboard_extra$Keyboard_Extra$Other,
		A2(_elm_lang$core$Dict$get, code, _ohanhi$keyboard_extra$Keyboard_Extra$codeDict));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$pressedDown = function (model) {
	return A2(
		_elm_lang$core$List$map,
		_ohanhi$keyboard_extra$Keyboard_Extra$fromCode,
		_elm_lang$core$Set$toList(model.keysDown));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$targetKey = A2(
	_elm_lang$core$Json_Decode$map,
	_ohanhi$keyboard_extra$Keyboard_Extra$fromCode,
	A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int));

var _user$project$Logos$viewClose = A2(
	_elm_lang$svg$Svg$svg,
	{
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$viewBox('0 0 7 7'),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$class('tab-close-svg'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('currentColor'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$strokeWidth('1.5'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$strokeLinecap('round'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeLinejoin('round'),
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$path,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$d('M6,6 L1,1'),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$path,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$d('M1,6 L6,1'),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				}
			}),
		_1: {ctor: '[]'}
	});
var _user$project$Logos$viewHaskellLogo = A2(
	_elm_lang$html$Html$div,
	{ctor: '[]'},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$svg$Svg$svg,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('tab-logo-svg'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$viewBox('0 0 481.8897 340.1574'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$version('1.1'),
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$defs,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$clipPath,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$id('clip1'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$svg$Svg$path,
									{
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$d('M 0 340.15625 L 481.890625 340.15625 L 481.890625 0 L 0 0 L 0 340.15625 Z M 0 340.15625 '),
										_1: {ctor: '[]'}
									},
									{ctor: '[]'}),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$g,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$id('surface0'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$g,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$clipPath('url(#clip1)'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$clipRule('nonzero'),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$svg$Svg$path,
										{
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$style(' stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$d('M 0 340.15625 L 113.386719 170.078125 L 0 0 L 85.039062 0 L 198.425781 170.078125 L 85.039062 340.15625 L 0 340.15625 Z M 0 340.15625 '),
												_1: {ctor: '[]'}
											}
										},
										{ctor: '[]'}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$svg$Svg$path,
											{
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$style(' stroke:none;fill-rule: nonzero; fill: rgb(60%,60%,60%); fill-opacity: 1;'),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$d('M 113.386719 340.15625 L 226.773438 170.078125 L 113.386719 0 L 198.425781 0 L 425.195312 340.15625 L 340.15625 340.15625 L 269.292969 233.859375 L 198.425781 340.15625 L 113.386719 340.15625 Z M 113.386719 340.15625 '),
													_1: {ctor: '[]'}
												}
											},
											{ctor: '[]'}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$svg$Svg$path,
												{
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$style(' stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;'),
													_1: {
														ctor: '::',
														_0: _elm_lang$svg$Svg_Attributes$d('M 387.402344 240.945312 L 349.609375 184.253906 L 481.890625 184.25 L 481.890625 240.945312 L 387.402344 240.945312 Z M 387.402344 240.945312 '),
														_1: {ctor: '[]'}
													}
												},
												{ctor: '[]'}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$svg$Svg$path,
													{
														ctor: '::',
														_0: _elm_lang$svg$Svg_Attributes$style(' stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;'),
														_1: {
															ctor: '::',
															_0: _elm_lang$svg$Svg_Attributes$d('M 330.710938 155.90625 L 292.914062 99.214844 L 481.890625 99.210938 L 481.890625 155.90625 L 330.710938 155.90625 Z M 330.710938 155.90625 '),
															_1: {ctor: '[]'}
														}
													},
													{ctor: '[]'}),
												_1: {ctor: '[]'}
											}
										}
									}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			}),
		_1: {ctor: '[]'}
	});
var _user$project$Logos$viewElixirLogo = A2(
	_elm_lang$html$Html$img,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('tab-logo-svg'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$src('assets/elixir-logo.png'),
			_1: {ctor: '[]'}
		}
	},
	{ctor: '[]'});
var _user$project$Logos$viewElmLogo = A2(
	_elm_lang$html$Html$div,
	{ctor: '[]'},
	{
		ctor: '::',
		_0: A2(
			_elm_lang$svg$Svg$svg,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$version('1.1'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$x('0'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$y('0'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$viewBox('0 0 323.141 322.95'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$class('tab-logo-svg'),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$polygon,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('#F0AD00'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$points('161.649,152.782 231.514,82.916 91.783,82.916'),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$polygon,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$fill('#7FD13B'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$points('8.867,0 79.241,70.375 232.213,70.375 161.838,0'),
								_1: {ctor: '[]'}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$rect,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill('#7FD13B'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$x('192.99'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$y('107.392'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$width('107.676'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$height('108.167'),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$transform('matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)'),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$polygon,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$fill('#60B5CC'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$points('323.298,143.724 323.298,0 179.573,0'),
										_1: {ctor: '[]'}
									}
								},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$svg$Svg$polygon,
									{
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$fill('#5A6378'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$points('152.781,161.649 0,8.868 0,314.432'),
											_1: {ctor: '[]'}
										}
									},
									{ctor: '[]'}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$svg$Svg$polygon,
										{
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$fill('#F0AD00'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$points('255.522,246.655 323.298,314.432 323.298,178.879'),
												_1: {ctor: '[]'}
											}
										},
										{ctor: '[]'}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$svg$Svg$polygon,
											{
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$fill('#60B5CC'),
												_1: {
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$points('161.649,170.517 8.869,323.298 314.43,323.298'),
													_1: {ctor: '[]'}
												}
											},
											{ctor: '[]'}),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}),
		_1: {ctor: '[]'}
	});

var _user$project$Reorderable_State$Point = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _user$project$Reorderable_State$Config = function (a) {
	return {animate: a};
};
var _user$project$Reorderable_State$State = F4(
	function (a, b, c, d) {
		return {placeholder: a, items: b, reorderedItems: c, animating: d};
	});
var _user$project$Reorderable_State$Placeholder = F4(
	function (a, b, c, d) {
		return {point: a, draggable: b, sourceIndex: c, destIndex: d};
	});
var _user$project$Reorderable_State$Bounds = F4(
	function (a, b, c, d) {
		return {top: a, left: b, bottom: c, right: d};
	});
var _user$project$Reorderable_State$Size = F2(
	function (a, b) {
		return {height: a, width: b};
	});
var _user$project$Reorderable_State$NonPlaceholderReorderable = function (a) {
	return {ctor: 'NonPlaceholderReorderable', _0: a};
};
var _user$project$Reorderable_State$PlaceholderReorderable = F2(
	function (a, b) {
		return {ctor: 'PlaceholderReorderable', _0: a, _1: b};
	});

var _user$project$Reorderable_Ports$scrollByPort = _elm_lang$core$Native_Platform.outgoingPort(
	'scrollByPort',
	function (v) {
		return [v._0, v._1];
	});
var _user$project$Reorderable_Ports$scrollBy = _elm_lang$core$Basics$curry(_user$project$Reorderable_Ports$scrollByPort);
var _user$project$Reorderable_Ports$dragStart = _elm_lang$core$Native_Platform.incomingPort(
	'dragStart',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (sourceIndex) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (point) {
					return _elm_lang$core$Json_Decode$succeed(
						{sourceIndex: sourceIndex, point: point});
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'point',
					A2(
						_elm_lang$core$Json_Decode$andThen,
						function (x) {
							return A2(
								_elm_lang$core$Json_Decode$andThen,
								function (y) {
									return _elm_lang$core$Json_Decode$succeed(
										{x: x, y: y});
								},
								A2(_elm_lang$core$Json_Decode$field, 'y', _elm_lang$core$Json_Decode$float));
						},
						A2(_elm_lang$core$Json_Decode$field, 'x', _elm_lang$core$Json_Decode$float))));
		},
		A2(_elm_lang$core$Json_Decode$field, 'sourceIndex', _elm_lang$core$Json_Decode$int)));
var _user$project$Reorderable_Ports$dragStop = _elm_lang$core$Native_Platform.incomingPort(
	'dragStop',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (placeholder) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (reorderableBounds) {
					return _elm_lang$core$Json_Decode$succeed(
						{placeholder: placeholder, reorderableBounds: reorderableBounds});
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'reorderableBounds',
					_elm_lang$core$Json_Decode$list(
						A2(
							_elm_lang$core$Json_Decode$andThen,
							function (top) {
								return A2(
									_elm_lang$core$Json_Decode$andThen,
									function (left) {
										return A2(
											_elm_lang$core$Json_Decode$andThen,
											function (bottom) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													function (right) {
														return _elm_lang$core$Json_Decode$succeed(
															{top: top, left: left, bottom: bottom, right: right});
													},
													A2(_elm_lang$core$Json_Decode$field, 'right', _elm_lang$core$Json_Decode$float));
											},
											A2(_elm_lang$core$Json_Decode$field, 'bottom', _elm_lang$core$Json_Decode$float));
									},
									A2(_elm_lang$core$Json_Decode$field, 'left', _elm_lang$core$Json_Decode$float));
							},
							A2(_elm_lang$core$Json_Decode$field, 'top', _elm_lang$core$Json_Decode$float)))));
		},
		A2(
			_elm_lang$core$Json_Decode$field,
			'placeholder',
			A2(
				_elm_lang$core$Json_Decode$andThen,
				function (point) {
					return A2(
						_elm_lang$core$Json_Decode$andThen,
						function (bounds) {
							return _elm_lang$core$Json_Decode$succeed(
								{point: point, bounds: bounds});
						},
						A2(
							_elm_lang$core$Json_Decode$field,
							'bounds',
							A2(
								_elm_lang$core$Json_Decode$andThen,
								function (top) {
									return A2(
										_elm_lang$core$Json_Decode$andThen,
										function (left) {
											return A2(
												_elm_lang$core$Json_Decode$andThen,
												function (bottom) {
													return A2(
														_elm_lang$core$Json_Decode$andThen,
														function (right) {
															return _elm_lang$core$Json_Decode$succeed(
																{top: top, left: left, bottom: bottom, right: right});
														},
														A2(_elm_lang$core$Json_Decode$field, 'right', _elm_lang$core$Json_Decode$float));
												},
												A2(_elm_lang$core$Json_Decode$field, 'bottom', _elm_lang$core$Json_Decode$float));
										},
										A2(_elm_lang$core$Json_Decode$field, 'left', _elm_lang$core$Json_Decode$float));
								},
								A2(_elm_lang$core$Json_Decode$field, 'top', _elm_lang$core$Json_Decode$float))));
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'point',
					A2(
						_elm_lang$core$Json_Decode$andThen,
						function (x) {
							return A2(
								_elm_lang$core$Json_Decode$andThen,
								function (y) {
									return _elm_lang$core$Json_Decode$succeed(
										{x: x, y: y});
								},
								A2(_elm_lang$core$Json_Decode$field, 'y', _elm_lang$core$Json_Decode$float));
						},
						A2(_elm_lang$core$Json_Decode$field, 'x', _elm_lang$core$Json_Decode$float)))))));
var _user$project$Reorderable_Ports$dragMove = _elm_lang$core$Native_Platform.incomingPort(
	'dragMove',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (clientSize) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (cursor) {
					return A2(
						_elm_lang$core$Json_Decode$andThen,
						function (placeholder) {
							return A2(
								_elm_lang$core$Json_Decode$andThen,
								function (reorderableBounds) {
									return _elm_lang$core$Json_Decode$succeed(
										{clientSize: clientSize, cursor: cursor, placeholder: placeholder, reorderableBounds: reorderableBounds});
								},
								A2(
									_elm_lang$core$Json_Decode$field,
									'reorderableBounds',
									_elm_lang$core$Json_Decode$list(
										A2(
											_elm_lang$core$Json_Decode$andThen,
											function (top) {
												return A2(
													_elm_lang$core$Json_Decode$andThen,
													function (left) {
														return A2(
															_elm_lang$core$Json_Decode$andThen,
															function (bottom) {
																return A2(
																	_elm_lang$core$Json_Decode$andThen,
																	function (right) {
																		return _elm_lang$core$Json_Decode$succeed(
																			{top: top, left: left, bottom: bottom, right: right});
																	},
																	A2(_elm_lang$core$Json_Decode$field, 'right', _elm_lang$core$Json_Decode$float));
															},
															A2(_elm_lang$core$Json_Decode$field, 'bottom', _elm_lang$core$Json_Decode$float));
													},
													A2(_elm_lang$core$Json_Decode$field, 'left', _elm_lang$core$Json_Decode$float));
											},
											A2(_elm_lang$core$Json_Decode$field, 'top', _elm_lang$core$Json_Decode$float)))));
						},
						A2(
							_elm_lang$core$Json_Decode$field,
							'placeholder',
							A2(
								_elm_lang$core$Json_Decode$andThen,
								function (point) {
									return A2(
										_elm_lang$core$Json_Decode$andThen,
										function (bounds) {
											return _elm_lang$core$Json_Decode$succeed(
												{point: point, bounds: bounds});
										},
										A2(
											_elm_lang$core$Json_Decode$field,
											'bounds',
											A2(
												_elm_lang$core$Json_Decode$andThen,
												function (top) {
													return A2(
														_elm_lang$core$Json_Decode$andThen,
														function (left) {
															return A2(
																_elm_lang$core$Json_Decode$andThen,
																function (bottom) {
																	return A2(
																		_elm_lang$core$Json_Decode$andThen,
																		function (right) {
																			return _elm_lang$core$Json_Decode$succeed(
																				{top: top, left: left, bottom: bottom, right: right});
																		},
																		A2(_elm_lang$core$Json_Decode$field, 'right', _elm_lang$core$Json_Decode$float));
																},
																A2(_elm_lang$core$Json_Decode$field, 'bottom', _elm_lang$core$Json_Decode$float));
														},
														A2(_elm_lang$core$Json_Decode$field, 'left', _elm_lang$core$Json_Decode$float));
												},
												A2(_elm_lang$core$Json_Decode$field, 'top', _elm_lang$core$Json_Decode$float))));
								},
								A2(
									_elm_lang$core$Json_Decode$field,
									'point',
									A2(
										_elm_lang$core$Json_Decode$andThen,
										function (x) {
											return A2(
												_elm_lang$core$Json_Decode$andThen,
												function (y) {
													return _elm_lang$core$Json_Decode$succeed(
														{x: x, y: y});
												},
												A2(_elm_lang$core$Json_Decode$field, 'y', _elm_lang$core$Json_Decode$float));
										},
										A2(_elm_lang$core$Json_Decode$field, 'x', _elm_lang$core$Json_Decode$float))))));
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'cursor',
					A2(
						_elm_lang$core$Json_Decode$andThen,
						function (x) {
							return A2(
								_elm_lang$core$Json_Decode$andThen,
								function (y) {
									return _elm_lang$core$Json_Decode$succeed(
										{x: x, y: y});
								},
								A2(_elm_lang$core$Json_Decode$field, 'y', _elm_lang$core$Json_Decode$float));
						},
						A2(_elm_lang$core$Json_Decode$field, 'x', _elm_lang$core$Json_Decode$float))));
		},
		A2(
			_elm_lang$core$Json_Decode$field,
			'clientSize',
			A2(
				_elm_lang$core$Json_Decode$andThen,
				function (height) {
					return A2(
						_elm_lang$core$Json_Decode$andThen,
						function (width) {
							return _elm_lang$core$Json_Decode$succeed(
								{height: height, width: width});
						},
						A2(_elm_lang$core$Json_Decode$field, 'width', _elm_lang$core$Json_Decode$float));
				},
				A2(_elm_lang$core$Json_Decode$field, 'height', _elm_lang$core$Json_Decode$float)))));

var _user$project$Reorderable_Update$dropAndShift = F3(
	function (sourceIndex, destIndex, items) {
		var restOfTabs = function (second) {
			return A3(
				_Skinney$elm_array_exploration$Array_Hamt$slice,
				second + 1,
				_Skinney$elm_array_exploration$Array_Hamt$length(items),
				items);
		};
		var itemsBetweenShiftLeft = F2(
			function (first, second) {
				return A3(_Skinney$elm_array_exploration$Array_Hamt$slice, first, second, items);
			});
		var itemsBetweenShiftRight = F2(
			function (first, second) {
				return A3(_Skinney$elm_array_exploration$Array_Hamt$slice, first + 1, second + 1, items);
			});
		var itemsUntilFirst = function (first) {
			return A3(_Skinney$elm_array_exploration$Array_Hamt$slice, 0, first, items);
		};
		var shiftLeft = F3(
			function (first, second, selected) {
				return A3(
					_elm_lang$core$Basics$flip,
					_Skinney$elm_array_exploration$Array_Hamt$append,
					restOfTabs(second),
					A3(
						_elm_lang$core$Basics$flip,
						_Skinney$elm_array_exploration$Array_Hamt$append,
						A2(itemsBetweenShiftLeft, first, second),
						A2(
							_Skinney$elm_array_exploration$Array_Hamt$push,
							selected,
							itemsUntilFirst(first))));
			});
		var shiftRight = F3(
			function (first, second, selected) {
				return A3(
					_elm_lang$core$Basics$flip,
					_Skinney$elm_array_exploration$Array_Hamt$append,
					restOfTabs(second),
					A2(
						_Skinney$elm_array_exploration$Array_Hamt$push,
						selected,
						A3(
							_elm_lang$core$Basics$flip,
							_Skinney$elm_array_exploration$Array_Hamt$append,
							A2(itemsBetweenShiftRight, first, second),
							itemsUntilFirst(first))));
			});
		var shift = function (tab) {
			return (_elm_lang$core$Native_Utils.cmp(sourceIndex, destIndex) < 0) ? A3(shiftRight, sourceIndex, destIndex, tab) : A3(shiftLeft, destIndex, sourceIndex, tab);
		};
		return A2(
			_elm_lang$core$Maybe$withDefault,
			items,
			A2(
				_elm_lang$core$Maybe$map,
				shift,
				A2(_Skinney$elm_array_exploration$Array_Hamt$get, sourceIndex, items)));
	});
var _user$project$Reorderable_Update$getOverlapArea = F2(
	function (d0, d1) {
		var yOverlap = A2(_elm_lang$core$Basics$min, d0.bottom, d1.bottom) - A2(_elm_lang$core$Basics$max, d0.top, d1.top);
		var xOverlap = A2(_elm_lang$core$Basics$min, d0.right, d1.right) - A2(_elm_lang$core$Basics$max, d0.left, d1.left);
		var overlap = A2(_elm_lang$core$Basics$max, 0, xOverlap * yOverlap);
		var overlapPlusLeft = A2(
			_elm_lang$core$Basics$max,
			0,
			overlap + (A2(_elm_lang$core$Basics$max, 0, d0.left - d1.left) * yOverlap));
		var d1Width = d1.right - d1.left;
		var d0Width = d0.right - d0.left;
		var halfWay = A2(_elm_lang$core$Basics$max, 0, (d1Width - d0Width) * yOverlap);
		return (_elm_lang$core$Native_Utils.cmp(d0Width, d1Width) < 0) ? (((_elm_lang$core$Native_Utils.cmp(overlapPlusLeft, halfWay) > -1) && (_elm_lang$core$Native_Utils.cmp(d0.right, d1.right) < 0)) ? {ctor: '_Tuple2', _0: d1Width * yOverlap, _1: false} : (((_elm_lang$core$Native_Utils.cmp(overlapPlusLeft, halfWay) < 0) && ((_elm_lang$core$Native_Utils.cmp(d0.right, d1.right) < 0) && (_elm_lang$core$Native_Utils.cmp(d0.left, d1.left) > 0))) ? {ctor: '_Tuple2', _0: 1, _1: true} : {ctor: '_Tuple2', _0: 0, _1: false})) : {ctor: '_Tuple2', _0: overlap, _1: false};
	});
var _user$project$Reorderable_Update$getMostOverlappingBoundsHelp = F5(
	function (draggableIndex, area, winner, elem, candidates) {
		getMostOverlappingBoundsHelp:
		while (true) {
			var _p0 = candidates;
			if (_p0.ctor === '[]') {
				return winner;
			} else {
				var _p2 = _p0._1;
				var _p1 = A2(_user$project$Reorderable_Update$getOverlapArea, elem, _p0._0);
				var candidateOverlapArea = _p1._0;
				var freeze = _p1._1;
				if (_elm_lang$core$Native_Utils.cmp(candidateOverlapArea, area) > 0) {
					var _v1 = draggableIndex + 1,
						_v2 = candidateOverlapArea,
						_v3 = _elm_lang$core$Maybe$Just(
						{ctor: '_Tuple2', _0: draggableIndex, _1: freeze}),
						_v4 = elem,
						_v5 = _p2;
					draggableIndex = _v1;
					area = _v2;
					winner = _v3;
					elem = _v4;
					candidates = _v5;
					continue getMostOverlappingBoundsHelp;
				} else {
					var _v6 = draggableIndex + 1,
						_v7 = area,
						_v8 = winner,
						_v9 = elem,
						_v10 = _p2;
					draggableIndex = _v6;
					area = _v7;
					winner = _v8;
					elem = _v9;
					candidates = _v10;
					continue getMostOverlappingBoundsHelp;
				}
			}
		}
	});
var _user$project$Reorderable_Update$getMostOverlappingBounds = A3(_user$project$Reorderable_Update$getMostOverlappingBoundsHelp, 0, 0, _elm_lang$core$Maybe$Nothing);
var _user$project$Reorderable_Update$autoscrollAmount = 10;
var _user$project$Reorderable_Update$autoscrollMaxPercentage = 0.2;
var _user$project$Reorderable_Update$update = F2(
	function (msg, dragInfo) {
		var _p3 = msg;
		switch (_p3.ctor) {
			case 'DragStart':
				var _p5 = _p3._0.sourceIndex;
				var _p4 = A2(_Skinney$elm_array_exploration$Array_Hamt$get, _p5, dragInfo.items);
				if (_p4.ctor === 'Just') {
					return _elm_lang$core$Result$Ok(
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								dragInfo,
								{
									placeholder: _elm_lang$core$Maybe$Just(
										{
											point: _p3._0.point,
											draggable: _p4._0,
											sourceIndex: _p5,
											destIndex: _elm_lang$core$Maybe$Just(_p5)
										}),
									reorderedItems: dragInfo.items
								}),
							_1: _elm_lang$core$Platform_Cmd$none
						});
				} else {
					return _elm_lang$core$Result$Err(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'Encountered a sourceIndex ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p5),
								A2(
									_elm_lang$core$Basics_ops['++'],
									' - which was not in dragInfo.items. This should never happen! dragInfo.items was: ',
									_elm_lang$core$Basics$toString(dragInfo.items)))));
				}
			case 'DragMove':
				var _p11 = _p3._0.placeholder;
				var _p10 = _p3._0.cursor;
				var _p9 = _p3._0.clientSize;
				var minBottomToScroll = _p9.height * (1 - _user$project$Reorderable_Update$autoscrollMaxPercentage);
				var maxTopToScroll = _p9.height * _user$project$Reorderable_Update$autoscrollMaxPercentage;
				var commands = (_elm_lang$core$Native_Utils.cmp(_p10.y, minBottomToScroll) > 0) ? {
					ctor: '::',
					_0: A2(_user$project$Reorderable_Ports$scrollBy, 0, _user$project$Reorderable_Update$autoscrollAmount),
					_1: {ctor: '[]'}
				} : ((_elm_lang$core$Native_Utils.cmp(_p10.y, maxTopToScroll) < 0) ? {
					ctor: '::',
					_0: A2(_user$project$Reorderable_Ports$scrollBy, 0, 0 - _user$project$Reorderable_Update$autoscrollAmount),
					_1: {ctor: '[]'}
				} : {ctor: '[]'});
				if (dragInfo.animating) {
					return _elm_lang$core$Result$Ok(
						{ctor: '_Tuple2', _0: dragInfo, _1: _elm_lang$core$Platform_Cmd$none});
				} else {
					var _p6 = dragInfo.placeholder;
					if (_p6.ctor === 'Nothing') {
						return _elm_lang$core$Result$Ok(
							{ctor: '_Tuple2', _0: dragInfo, _1: _elm_lang$core$Platform_Cmd$none});
					} else {
						var _p8 = _p6._0.sourceIndex;
						var maybeReorderableIndex = A2(_user$project$Reorderable_Update$getMostOverlappingBounds, _p11.bounds, _p3._0.reorderableBounds);
						var newPlaceholder = {
							point: _p11.point,
							draggable: _p6._0.draggable,
							sourceIndex: _p8,
							destIndex: A2(_elm_lang$core$Maybe$map, _elm_lang$core$Tuple$first, maybeReorderableIndex)
						};
						var reorderedItems = function () {
							var _p7 = maybeReorderableIndex;
							if (_p7.ctor === 'Just') {
								return _p7._0._1 ? dragInfo.reorderedItems : A3(_user$project$Reorderable_Update$dropAndShift, _p8, _p7._0._0, dragInfo.items);
							} else {
								return dragInfo.items;
							}
						}();
						return _elm_lang$core$Result$Ok(
							{
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									dragInfo,
									{
										placeholder: _elm_lang$core$Maybe$Just(newPlaceholder),
										reorderedItems: reorderedItems
									}),
								_1: _elm_lang$core$Platform_Cmd$batch(commands)
							});
					}
				}
			case 'DragStop':
				var _p12 = dragInfo.placeholder;
				if (_p12.ctor === 'Nothing') {
					return _elm_lang$core$Result$Err('Received a DragStop when dragInfo.placeholder was Nothing. This should never happen!');
				} else {
					var _p13 = _p12._0.destIndex;
					if (_p13.ctor === 'Nothing') {
						return _elm_lang$core$Result$Ok(
							{
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									dragInfo,
									{placeholder: _elm_lang$core$Maybe$Nothing}),
								_1: _elm_lang$core$Platform_Cmd$none
							});
					} else {
						var newReorderables = A3(_user$project$Reorderable_Update$dropAndShift, _p12._0.sourceIndex, _p13._0, dragInfo.items);
						return _elm_lang$core$Result$Ok(
							{
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									dragInfo,
									{placeholder: _elm_lang$core$Maybe$Nothing, items: newReorderables, reorderedItems: newReorderables, animating: false}),
								_1: _elm_lang$core$Platform_Cmd$none
							});
					}
				}
			default:
				return _elm_lang$core$Result$Ok(
					{
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							dragInfo,
							{animating: true}),
						_1: _elm_lang$core$Platform_Cmd$none
					});
		}
	});
var _user$project$Reorderable_Update$DragMove = function (a) {
	return {ctor: 'DragMove', _0: a};
};
var _user$project$Reorderable_Update$DragStop = {ctor: 'DragStop'};
var _user$project$Reorderable_Update$DragHold = function (a) {
	return {ctor: 'DragHold', _0: a};
};
var _user$project$Reorderable_Update$DragStart = function (a) {
	return {ctor: 'DragStart', _0: a};
};
var _user$project$Reorderable_Update$subscriptions = F2(
	function (config, state) {
		return _elm_lang$core$Native_Utils.eq(state.placeholder, _elm_lang$core$Maybe$Nothing) ? _user$project$Reorderable_Ports$dragStart(_user$project$Reorderable_Update$DragStart) : _elm_lang$core$Platform_Sub$batch(
			{
				ctor: '::',
				_0: _user$project$Reorderable_Ports$dragMove(_user$project$Reorderable_Update$DragMove),
				_1: {
					ctor: '::',
					_0: _user$project$Reorderable_Ports$dragStop(
						function (moveData) {
							return config.animate ? _user$project$Reorderable_Update$DragHold(moveData) : _user$project$Reorderable_Update$DragStop;
						}),
					_1: {ctor: '[]'}
				}
			});
	});

var _user$project$Types$dragConfig = {animate: true};
var _user$project$Types$tabMenuItemToString = function (menuItem) {
	var _p0 = menuItem;
	switch (_p0.ctor) {
		case 'PinTab':
			return 'Pin Tab';
		case 'UnpinTab':
			return 'Unpin Tab';
		case 'CloseTab':
			return 'Close Tab';
		case 'CloseOtherTabs':
			return 'Close Other Tabs';
		default:
			return 'Close Tabs to the Right';
	}
};
var _user$project$Types$SlidingPlaceholder = F5(
	function (a, b, c, d, e) {
		return {start: a, end: b, sourceTabIndex: c, destTabIndex: d, tab: e};
	});
var _user$project$Types$PinningPlaceholder = F6(
	function (a, b, c, d, e, f) {
		return {start: a, end: b, startWidth: c, oldTabIndex: d, newTabIndex: e, tab: f};
	});
var _user$project$Types$UnPinningPlaceholder = F6(
	function (a, b, c, d, e, f) {
		return {start: a, end: b, endWidth: c, oldTabIndex: d, newTabIndex: e, tab: f};
	});
var _user$project$Types$TabMenu = F4(
	function (a, b, c, d) {
		return {tabIndex: a, position: b, tabRect: c, tab: d};
	});
var _user$project$Types$Tab = F4(
	function (a, b, c, d) {
		return {id: a, title: b, icon: c, isPinned: d};
	});
var _user$project$Types$TabClickInfo = F2(
	function (a, b) {
		return {position: a, rect: b};
	});
var _user$project$Types$UnPinning = function (a) {
	return {ctor: 'UnPinning', _0: a};
};
var _user$project$Types$Pinning = function (a) {
	return {ctor: 'Pinning', _0: a};
};
var _user$project$Types$PinDestBackdrop = {ctor: 'PinDestBackdrop'};
var _user$project$Types$UnPinSourceBackdrop = {ctor: 'UnPinSourceBackdrop'};
var _user$project$Types$PinSourceBackdrop = {ctor: 'PinSourceBackdrop'};
var _user$project$Types$DropPreview = {ctor: 'DropPreview'};
var _user$project$Types$ReorderableTab = {ctor: 'ReorderableTab'};
var _user$project$Types$CloseTabsToTheRight = function (a) {
	return {ctor: 'CloseTabsToTheRight', _0: a};
};
var _user$project$Types$CloseOtherTabs = function (a) {
	return {ctor: 'CloseOtherTabs', _0: a};
};
var _user$project$Types$CloseTab = function (a) {
	return {ctor: 'CloseTab', _0: a};
};
var _user$project$Types$UnpinTab = function (a) {
	return {ctor: 'UnpinTab', _0: a};
};
var _user$project$Types$PinTab = function (a) {
	return {ctor: 'PinTab', _0: a};
};
var _user$project$Types$tabMenuItems = F2(
	function (isPinned, tabIndex) {
		return {
			ctor: '::',
			_0: isPinned ? _user$project$Types$UnpinTab(tabIndex) : _user$project$Types$PinTab(tabIndex),
			_1: {
				ctor: '::',
				_0: _user$project$Types$CloseTab(tabIndex),
				_1: {
					ctor: '::',
					_0: _user$project$Types$CloseOtherTabs(tabIndex),
					_1: {
						ctor: '::',
						_0: _user$project$Types$CloseTabsToTheRight(tabIndex),
						_1: {ctor: '[]'}
					}
				}
			}
		};
	});
var _user$project$Types$Haskell = {ctor: 'Haskell'};
var _user$project$Types$Elixir = {ctor: 'Elixir'};
var _user$project$Types$Elm = {ctor: 'Elm'};
var _user$project$Types$toLogo = function (str) {
	var _p1 = str;
	switch (_p1) {
		case 'Elm':
			return _user$project$Types$Elm;
		case 'Elixir':
			return _user$project$Types$Elixir;
		case 'Haskell':
			return _user$project$Types$Haskell;
		default:
			return _user$project$Types$Elm;
	}
};

var _user$project$Messages$WindowResize = function (a) {
	return {ctor: 'WindowResize', _0: a};
};
var _user$project$Messages$NewTabWidth = function (a) {
	return {ctor: 'NewTabWidth', _0: a};
};
var _user$project$Messages$AddTab = {ctor: 'AddTab'};
var _user$project$Messages$CloseAllMenus = {ctor: 'CloseAllMenus'};
var _user$project$Messages$CloseTabsToTheRightOfIndex = function (a) {
	return {ctor: 'CloseTabsToTheRightOfIndex', _0: a};
};
var _user$project$Messages$CloseTabsOtherThanIndex = function (a) {
	return {ctor: 'CloseTabsOtherThanIndex', _0: a};
};
var _user$project$Messages$CloseTabAtIndex = function (a) {
	return {ctor: 'CloseTabAtIndex', _0: a};
};
var _user$project$Messages$FinishUnpinningTab = function (a) {
	return {ctor: 'FinishUnpinningTab', _0: a};
};
var _user$project$Messages$UnpinTabAtIndex = F3(
	function (a, b, c) {
		return {ctor: 'UnpinTabAtIndex', _0: a, _1: b, _2: c};
	});
var _user$project$Messages$FinishPinningTab = function (a) {
	return {ctor: 'FinishPinningTab', _0: a};
};
var _user$project$Messages$PinTabAtIndex = F3(
	function (a, b, c) {
		return {ctor: 'PinTabAtIndex', _0: a, _1: b, _2: c};
	});
var _user$project$Messages$ToggleTabMenu = F2(
	function (a, b) {
		return {ctor: 'ToggleTabMenu', _0: a, _1: b};
	});
var _user$project$Messages$SetActive = function (a) {
	return {ctor: 'SetActive', _0: a};
};
var _user$project$Messages$FinishSlidingTab = function (a) {
	return {ctor: 'FinishSlidingTab', _0: a};
};
var _user$project$Messages$DragMsg = function (a) {
	return {ctor: 'DragMsg', _0: a};
};
var _user$project$Messages$KeyboardExtraMsg = function (a) {
	return {ctor: 'KeyboardExtraMsg', _0: a};
};
var _user$project$Messages$AnimateMessenger = function (a) {
	return {ctor: 'AnimateMessenger', _0: a};
};

var _user$project$Model$titleFromId = function (id) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'Tab ',
		_elm_lang$core$Basics$toString(id));
};
var _user$project$Model$tabFromId = function (id) {
	var icon = _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], id, 3),
		0) ? 'Elm' : (_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], id, 3),
		1) ? 'Elixir' : 'Haskell');
	return {
		id: id + 1,
		title: _user$project$Model$titleFromId(id + 1),
		icon: icon,
		isPinned: false
	};
};
var _user$project$Model$someTabs = A2(_Skinney$elm_array_exploration$Array_Hamt$initialize, 5, _user$project$Model$tabFromId);
var _user$project$Model$initDragState = function (tabs) {
	return {placeholder: _elm_lang$core$Maybe$Nothing, items: tabs, reorderedItems: tabs, animating: false};
};
var _user$project$Model$initialModel = function () {
	var _p0 = _ohanhi$keyboard_extra$Keyboard_Extra$init;
	var keyboardModel = _p0._0;
	return {
		selected: {id: 1, title: 'Tab 1', icon: 'Elm', isPinned: false},
		pinPlaceholder: _elm_lang$core$Maybe$Nothing,
		pinPlaceholderStyle: _mdgriffith$elm_style_animation$Animation$style(
			{ctor: '[]'}),
		placeholderAnimationStyle: _mdgriffith$elm_style_animation$Animation$style(
			{ctor: '[]'}),
		tabMenu: _elm_lang$core$Maybe$Nothing,
		keyboardModel: keyboardModel,
		pinDestinationStyle: _mdgriffith$elm_style_animation$Animation$style(
			{ctor: '[]'}),
		pinStartBackdropStyle: _mdgriffith$elm_style_animation$Animation$style(
			{ctor: '[]'}),
		dragState: _user$project$Model$initDragState(_user$project$Model$someTabs),
		flexTabWidth: 0,
		pinnedTabWidth: 60,
		showingAnyMenu: false,
		nextTabId: _Skinney$elm_array_exploration$Array_Hamt$length(_user$project$Model$someTabs)
	};
}();
var _user$project$Model$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return {selected: a, pinPlaceholder: b, pinPlaceholderStyle: c, placeholderAnimationStyle: d, tabMenu: e, keyboardModel: f, pinDestinationStyle: g, pinStartBackdropStyle: h, dragState: i, flexTabWidth: j, pinnedTabWidth: k, showingAnyMenu: l, nextTabId: m};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};

var _user$project$Ports$getFlexTabWidth = _elm_lang$core$Native_Platform.outgoingPort(
	'getFlexTabWidth',
	function (v) {
		return v;
	});

var _user$project$Subscriptions$newTabWidth = _elm_lang$core$Native_Platform.incomingPort('newTabWidth', _elm_lang$core$Json_Decode$float);
var _user$project$Subscriptions$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Platform_Sub$map, _user$project$Messages$KeyboardExtraMsg, _ohanhi$keyboard_extra$Keyboard_Extra$subscriptions),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$core$Platform_Sub$map,
					_user$project$Messages$DragMsg,
					A2(_user$project$Reorderable_Update$subscriptions, _user$project$Types$dragConfig, model.dragState)),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$subscription,
						_user$project$Messages$AnimateMessenger,
						{
							ctor: '::',
							_0: model.pinPlaceholderStyle,
							_1: {
								ctor: '::',
								_0: model.placeholderAnimationStyle,
								_1: {
									ctor: '::',
									_0: model.pinDestinationStyle,
									_1: {
										ctor: '::',
										_0: model.pinStartBackdropStyle,
										_1: {ctor: '[]'}
									}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: _user$project$Subscriptions$newTabWidth(_user$project$Messages$NewTabWidth),
						_1: {
							ctor: '::',
							_0: _elm_lang$window$Window$resizes(_user$project$Messages$WindowResize),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		});
};

var _user$project$Util$fastDrift = _mdgriffith$elm_style_animation$Animation$easing(
	{
		duration: 0.1 * _elm_lang$core$Time$second,
		ease: function (x) {
			return Math.pow(x, 2);
		}
	});
var _user$project$Util$defaultPrevented = A2(_elm_lang$html$Html_Events$Options, false, true);
var _user$project$Util$getNumPinned = function (tabs) {
	return _Skinney$elm_array_exploration$Array_Hamt$length(
		A2(
			_Skinney$elm_array_exploration$Array_Hamt$filter,
			function (_) {
				return _.isPinned;
			},
			tabs));
};
var _user$project$Util$removeAtIndex = F2(
	function (index, array) {
		return A2(
			_Skinney$elm_array_exploration$Array_Hamt$append,
			A3(_Skinney$elm_array_exploration$Array_Hamt$slice, 0, index, array),
			A3(
				_Skinney$elm_array_exploration$Array_Hamt$slice,
				index + 1,
				_Skinney$elm_array_exploration$Array_Hamt$length(array) + 1,
				array));
	});
var _user$project$Util$toPx = function (num) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(num),
		'px');
};

var _user$project$Update$addTab = F2(
	function (tab, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: _user$project$Model$initDragState(
					A2(_Skinney$elm_array_exploration$Array_Hamt$push, tab, model.dragState.items)),
				nextTabId: model.nextTabId + 1
			});
	});
var _user$project$Update$closeTabsToTheRightOfIndex = F2(
	function (tabIndex, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: _user$project$Model$initDragState(
					A3(_Skinney$elm_array_exploration$Array_Hamt$slice, 0, tabIndex + 1, model.dragState.items))
			});
	});
var _user$project$Update$closeOtherTabsThanIndexHelp = F2(
	function (model, selectedTab) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: _user$project$Model$initDragState(
					_Skinney$elm_array_exploration$Array_Hamt$fromList(
						{
							ctor: '::',
							_0: selectedTab,
							_1: {ctor: '[]'}
						}))
			});
	});
var _user$project$Update$closeTabsOtherThanIndex = F2(
	function (tabIndex, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			model,
			A2(
				_elm_lang$core$Maybe$map,
				_user$project$Update$closeOtherTabsThanIndexHelp(model),
				A2(_Skinney$elm_array_exploration$Array_Hamt$get, tabIndex, model.dragState.items)));
	});
var _user$project$Update$closeTab = F2(
	function (indexToClose, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: _user$project$Model$initDragState(
					A2(_user$project$Util$removeAtIndex, indexToClose, model.dragState.items))
			});
	});
var _user$project$Update$startUnPinTabAnimation = F2(
	function (_p0, model) {
		var _p1 = _p0;
		var _p2 = _p1;
		var newStartingPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$width(
							_mdgriffith$elm_style_animation$Animation$px(0)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(model.flexTabWidth)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			model.pinStartBackdropStyle);
		var newTargetPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$width(
							_mdgriffith$elm_style_animation$Animation$px(model.pinnedTabWidth)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(0)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			model.pinDestinationStyle);
		var newPinPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$left(
							_mdgriffith$elm_style_animation$Animation$px(_p1.start.x)),
						_1: {
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(model.pinnedTabWidth)),
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$left(
								_mdgriffith$elm_style_animation$Animation$px(_p1.end.x)),
							_1: {
								ctor: '::',
								_0: _mdgriffith$elm_style_animation$Animation$width(
									_mdgriffith$elm_style_animation$Animation$px(_p1.endWidth)),
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation_Messenger$send(
							_user$project$Messages$FinishUnpinningTab(_p2)),
						_1: {ctor: '[]'}
					}
				}
			},
			model.pinPlaceholderStyle);
		var numTabs = _Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items);
		var numPinned = _user$project$Util$getNumPinned(model.dragState.items) + 1;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				pinPlaceholderStyle: newPinPlaceholderStyle,
				pinPlaceholder: _elm_lang$core$Maybe$Just(
					_user$project$Types$UnPinning(_p2)),
				pinDestinationStyle: newStartingPlaceholderStyle,
				pinStartBackdropStyle: newTargetPlaceholderStyle
			});
	});
var _user$project$Update$unPinTabHelp = F5(
	function (oldTabIndex, newTabIndex, tabs, model, tabToUnPin) {
		var unpinnedIndex = _user$project$Util$getNumPinned(model.dragState.items);
		var unpinnedTab = _elm_lang$core$Native_Utils.update(
			tabToUnPin,
			{isPinned: false});
		var newTabs = A3(
			_elm_lang$core$Basics$flip,
			_Skinney$elm_array_exploration$Array_Hamt$append,
			A3(
				_Skinney$elm_array_exploration$Array_Hamt$slice,
				newTabIndex,
				_Skinney$elm_array_exploration$Array_Hamt$length(tabs),
				tabs),
			A2(
				_Skinney$elm_array_exploration$Array_Hamt$push,
				unpinnedTab,
				A3(
					_elm_lang$core$Basics$flip,
					_Skinney$elm_array_exploration$Array_Hamt$append,
					A3(_Skinney$elm_array_exploration$Array_Hamt$slice, oldTabIndex + 1, newTabIndex, tabs),
					A3(_Skinney$elm_array_exploration$Array_Hamt$slice, 0, oldTabIndex, tabs))));
		var newDragState = _user$project$Model$initDragState(newTabs);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: newDragState,
				pinPlaceholder: _elm_lang$core$Maybe$Nothing,
				pinPlaceholderStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'}),
				pinStartBackdropStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'}),
				pinDestinationStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'})
			});
	});
var _user$project$Update$unpinTab = F3(
	function (oldTabIndex, newTabIndex, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			model,
			A2(
				_elm_lang$core$Maybe$map,
				A4(_user$project$Update$unPinTabHelp, oldTabIndex, newTabIndex, model.dragState.items, model),
				A2(_Skinney$elm_array_exploration$Array_Hamt$get, oldTabIndex, model.dragState.items)));
	});
var _user$project$Update$startPinTabAnimation = F2(
	function (_p3, model) {
		var _p4 = _p3;
		var _p6 = _p4.startWidth;
		var _p5 = _p4;
		var newStartingPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$width(
							_mdgriffith$elm_style_animation$Animation$px(0)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(model.pinnedTabWidth)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			model.pinStartBackdropStyle);
		var newTargetPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$width(
							_mdgriffith$elm_style_animation$Animation$px(_p6)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(0)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			model.pinDestinationStyle);
		var newPinPlaceholderStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$left(
							_mdgriffith$elm_style_animation$Animation$px(_p4.start.x)),
						_1: {
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$width(
								_mdgriffith$elm_style_animation$Animation$px(_p6)),
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$left(
								_mdgriffith$elm_style_animation$Animation$px(_p4.end.x)),
							_1: {
								ctor: '::',
								_0: _mdgriffith$elm_style_animation$Animation$width(
									_mdgriffith$elm_style_animation$Animation$px(model.pinnedTabWidth)),
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation_Messenger$send(
							_user$project$Messages$FinishPinningTab(_p5)),
						_1: {ctor: '[]'}
					}
				}
			},
			model.pinPlaceholderStyle);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				pinPlaceholderStyle: newPinPlaceholderStyle,
				flexTabWidth: _p6,
				pinPlaceholder: _elm_lang$core$Maybe$Just(
					_user$project$Types$Pinning(_p5)),
				pinDestinationStyle: newStartingPlaceholderStyle,
				pinStartBackdropStyle: newTargetPlaceholderStyle
			});
	});
var _user$project$Update$pinTabHelp = F4(
	function (oldTabIndex, tabs, model, tabToPin) {
		var unpinnedIndex = _user$project$Util$getNumPinned(tabs);
		var pinnedTab = _elm_lang$core$Native_Utils.update(
			tabToPin,
			{isPinned: true});
		var newTabs = A3(
			_elm_lang$core$Basics$flip,
			_Skinney$elm_array_exploration$Array_Hamt$append,
			A3(
				_Skinney$elm_array_exploration$Array_Hamt$slice,
				oldTabIndex + 1,
				_Skinney$elm_array_exploration$Array_Hamt$length(tabs),
				tabs),
			A3(
				_elm_lang$core$Basics$flip,
				_Skinney$elm_array_exploration$Array_Hamt$append,
				A3(_Skinney$elm_array_exploration$Array_Hamt$slice, unpinnedIndex, oldTabIndex, tabs),
				A2(
					_Skinney$elm_array_exploration$Array_Hamt$push,
					pinnedTab,
					A3(_Skinney$elm_array_exploration$Array_Hamt$slice, 0, unpinnedIndex, tabs))));
		var newDragState = _user$project$Model$initDragState(newTabs);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				dragState: newDragState,
				pinPlaceholder: _elm_lang$core$Maybe$Nothing,
				pinPlaceholderStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'}),
				pinStartBackdropStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'}),
				pinDestinationStyle: _mdgriffith$elm_style_animation$Animation$style(
					{ctor: '[]'})
			});
	});
var _user$project$Update$pinTab = F2(
	function (oldTabIndex, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			model,
			A2(
				_elm_lang$core$Maybe$map,
				A3(_user$project$Update$pinTabHelp, oldTabIndex, model.dragState.items, model),
				A2(_Skinney$elm_array_exploration$Array_Hamt$get, oldTabIndex, model.dragState.items)));
	});
var _user$project$Update$determineDestHelp = F5(
	function (numPinned, reorderableBounds, placeholderIsPinned, index, tab) {
		return (placeholderIsPinned && (!tab.isPinned)) ? (numPinned - 1) : (((!placeholderIsPinned) && tab.isPinned) ? numPinned : index);
	});
var _user$project$Update$determineDestIndex = F5(
	function (tabs, numPinned, reorderableBounds, placeholderIsPinned, index) {
		var helpDetermineIndex = A4(_user$project$Update$determineDestHelp, numPinned, reorderableBounds, placeholderIsPinned, index);
		return A2(
			_elm_lang$core$Maybe$map,
			helpDetermineIndex,
			A2(_Skinney$elm_array_exploration$Array_Hamt$get, index, tabs));
	});
var _user$project$Update$getDestForPinnableTabs = F4(
	function (tab, tabs, reorderableBounds, maybeDestIndex) {
		var numPinned = _user$project$Util$getNumPinned(tabs);
		var determinePlaceholderDestination = A4(_user$project$Update$determineDestIndex, tabs, numPinned, reorderableBounds, tab.isPinned);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			0,
			A2(_elm_lang$core$Maybe$andThen, determinePlaceholderDestination, maybeDestIndex));
	});
var _user$project$Update$resetDragState = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{
			dragState: _user$project$Model$initDragState(model.dragState.items)
		});
};
var _user$project$Update$closeAllMenus = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{tabMenu: _elm_lang$core$Maybe$Nothing, showingAnyMenu: false});
};
var _user$project$Update$initUnPinningInfo = F4(
	function (tabIndex, tab, _p7, model) {
		var _p8 = _p7;
		var _p10 = _p8.top;
		var _p9 = _p8.left;
		var numPinned = _user$project$Util$getNumPinned(model.dragState.items);
		var numPinnedTabsToTravel = (numPinned - tabIndex) - 1;
		var newTabPosition = A2(
			_user$project$Reorderable_State$Point,
			_p9 + (_elm_lang$core$Basics$toFloat(numPinnedTabsToTravel) * model.pinnedTabWidth),
			_p10);
		var tabPosition = A2(_user$project$Reorderable_State$Point, _p9, _p10);
		return {start: tabPosition, end: newTabPosition, endWidth: model.flexTabWidth, oldTabIndex: tabIndex, newTabIndex: numPinned, tab: tab};
	});
var _user$project$Update$initPinningInfo = F4(
	function (tabIndex, tab, _p11, model) {
		var _p12 = _p11;
		var _p15 = _p12.width;
		var _p14 = _p12.top;
		var _p13 = _p12.left;
		var newTabIndex = _user$project$Util$getNumPinned(model.dragState.items);
		var numUnPinned = tabIndex - newTabIndex;
		var pinDisplacement = _elm_lang$core$Basics$toFloat(numUnPinned) * _p15;
		var newTabPosition = A2(_user$project$Reorderable_State$Point, _p13 - pinDisplacement, _p14);
		var tabPosition = A2(_user$project$Reorderable_State$Point, _p13, _p14);
		return {start: tabPosition, end: newTabPosition, startWidth: _p15, oldTabIndex: tabIndex, newTabIndex: newTabIndex, tab: tab};
	});
var _user$project$Update$setShowingAnyMenuFalse = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{showingAnyMenu: false});
};
var _user$project$Update$setShowingAnyMenuTrue = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{showingAnyMenu: true});
};
var _user$project$Update$toggleTabMenuHelp = F4(
	function (model, tabIndex, _p16, tab) {
		var _p17 = _p16;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				tabMenu: _elm_lang$core$Native_Utils.eq(model.tabMenu, _elm_lang$core$Maybe$Nothing) ? _elm_lang$core$Maybe$Just(
					{tabIndex: tabIndex, position: _p17.position, tabRect: _p17.rect, tab: tab}) : _elm_lang$core$Maybe$Nothing
			});
	});
var _user$project$Update$toggleTabMenu = F3(
	function (tabIndex, tabClickInfo, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			model,
			A2(
				_elm_lang$core$Maybe$map,
				A3(_user$project$Update$toggleTabMenuHelp, model, tabIndex, tabClickInfo),
				A2(_Skinney$elm_array_exploration$Array_Hamt$get, tabIndex, model.dragState.items)));
	});
var _user$project$Update$setActiveTab = F2(
	function (tab, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{selected: tab});
	});
var _user$project$Update$startSlidingTabAnimation = F2(
	function (_p18, model) {
		var _p19 = _p18;
		var placeholderAnimationStyle = A2(
			_mdgriffith$elm_style_animation$Animation$interrupt,
			{
				ctor: '::',
				_0: _mdgriffith$elm_style_animation$Animation$set(
					{
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation$left(
							_mdgriffith$elm_style_animation$Animation$px(_p19.start.x)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_mdgriffith$elm_style_animation$Animation$toWith,
						_user$project$Util$fastDrift,
						{
							ctor: '::',
							_0: _mdgriffith$elm_style_animation$Animation$left(
								_mdgriffith$elm_style_animation$Animation$px(_p19.end.x)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _mdgriffith$elm_style_animation$Animation_Messenger$send(
							_user$project$Messages$FinishSlidingTab(_p19)),
						_1: {ctor: '[]'}
					}
				}
			},
			model.placeholderAnimationStyle);
		return _elm_lang$core$Native_Utils.update(
			model,
			{placeholderAnimationStyle: placeholderAnimationStyle});
	});
var _user$project$Update$slideTabIntoPreviewHelp = F4(
	function (model, placeholderBounds, reorderableBounds, _p20) {
		var _p21 = _p20;
		var _p22 = _p21.draggable;
		var start = A2(_user$project$Reorderable_State$Point, placeholderBounds.left, placeholderBounds.top);
		var maybeDestIndex = A2(_user$project$Reorderable_Update$getMostOverlappingBounds, placeholderBounds, reorderableBounds);
		var destTabIndex = A4(
			_user$project$Update$getDestForPinnableTabs,
			_p22,
			model.dragState.items,
			reorderableBounds,
			A2(_elm_lang$core$Maybe$map, _elm_lang$core$Tuple$first, maybeDestIndex));
		var destPreviewBounds = A2(
			_elm_lang$core$Maybe$withDefault,
			A4(_user$project$Reorderable_State$Bounds, 0, 0, 0, 0),
			_elm_lang$core$List$head(
				A2(_elm_lang$core$List$drop, destTabIndex, reorderableBounds)));
		var end = A2(_user$project$Reorderable_State$Point, destPreviewBounds.left, destPreviewBounds.top);
		return A2(
			_user$project$Update$startSlidingTabAnimation,
			{start: start, end: end, sourceTabIndex: _p21.sourceIndex, destTabIndex: destTabIndex, tab: _p22},
			model);
	});
var _user$project$Update$slideTabIntoPreview = F3(
	function (placeholderBounds, reorderableBounds, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			model,
			A2(
				_elm_lang$core$Maybe$map,
				A3(_user$project$Update$slideTabIntoPreviewHelp, model, placeholderBounds, reorderableBounds),
				model.dragState.placeholder));
	});
var _user$project$Update$updateDrag = F2(
	function (dragMsg, model) {
		var _p23 = A2(_user$project$Reorderable_Update$update, dragMsg, model.dragState);
		if (_p23.ctor === 'Ok') {
			var _p25 = _p23._0._0;
			var _p24 = dragMsg;
			if (_p24.ctor === 'DragHold') {
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					A3(
						_user$project$Update$slideTabIntoPreview,
						_p24._0.placeholder.bounds,
						_p24._0.reorderableBounds,
						_elm_lang$core$Native_Utils.update(
							model,
							{dragState: _p25})),
					{ctor: '[]'});
			} else {
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_elm_lang$core$Native_Utils.update(
						model,
						{dragState: _p25}),
					{
						ctor: '::',
						_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Messages$DragMsg, _p23._0._1),
						_1: {ctor: '[]'}
					});
			}
		} else {
			return A2(
				_NoRedInk$rocket_update$Rocket_ops['=>'],
				model,
				{ctor: '[]'});
		}
	});
var _user$project$Update$update = F2(
	function (msg, model) {
		var _p26 = msg;
		switch (_p26.ctor) {
			case 'AnimateMessenger':
				var _p31 = _p26._0;
				var _p27 = A2(_mdgriffith$elm_style_animation$Animation_Messenger$update, _p31, model.pinStartBackdropStyle);
				var pinStartBackdropStyle = _p27._0;
				var cmdsStartingPreview = _p27._1;
				var _p28 = A2(_mdgriffith$elm_style_animation$Animation_Messenger$update, _p31, model.pinDestinationStyle);
				var pinDestinationStyle = _p28._0;
				var cmdsTargetPreview = _p28._1;
				var _p29 = A2(_mdgriffith$elm_style_animation$Animation_Messenger$update, _p31, model.pinPlaceholderStyle);
				var pinPlaceholderStyle = _p29._0;
				var cmdsMoving = _p29._1;
				var _p30 = A2(_mdgriffith$elm_style_animation$Animation_Messenger$update, _p31, model.placeholderAnimationStyle);
				var placeholderAnimationStyle = _p30._0;
				var cmdsPlaceholder = _p30._1;
				var cmds = {
					ctor: '::',
					_0: cmdsMoving,
					_1: {
						ctor: '::',
						_0: cmdsPlaceholder,
						_1: {
							ctor: '::',
							_0: cmdsTargetPreview,
							_1: {
								ctor: '::',
								_0: cmdsStartingPreview,
								_1: {ctor: '[]'}
							}
						}
					}
				};
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_elm_lang$core$Native_Utils.update(
						model,
						{pinPlaceholderStyle: pinPlaceholderStyle, placeholderAnimationStyle: placeholderAnimationStyle, pinDestinationStyle: pinDestinationStyle, pinStartBackdropStyle: pinStartBackdropStyle}),
					cmds);
			case 'KeyboardExtraMsg':
				var _p32 = A2(_ohanhi$keyboard_extra$Keyboard_Extra$update, _p26._0, model.keyboardModel);
				var keyboardModel = _p32._0;
				var keyboardCmd = _p32._1;
				var escapeIsPressed = A2(_ohanhi$keyboard_extra$Keyboard_Extra$isPressed, _ohanhi$keyboard_extra$Keyboard_Extra$Escape, keyboardModel);
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$resetDragState(
						_user$project$Update$closeAllMenus(
							_elm_lang$core$Native_Utils.update(
								model,
								{keyboardModel: keyboardModel}))),
					{
						ctor: '::',
						_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Messages$KeyboardExtraMsg, keyboardCmd),
						_1: {ctor: '[]'}
					});
			case 'DragMsg':
				return A2(_user$project$Update$updateDrag, _p26._0, model);
			case 'FinishSlidingTab':
				var newTabs = A3(_user$project$Reorderable_Update$dropAndShift, _p26._0.sourceTabIndex, _p26._0.destTabIndex, model.dragState.items);
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							placeholderAnimationStyle: _mdgriffith$elm_style_animation$Animation$style(
								{ctor: '[]'}),
							dragState: _user$project$Model$initDragState(newTabs)
						}),
					{ctor: '[]'});
			case 'SetActive':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					A2(_user$project$Update$setActiveTab, _p26._0, model),
					{ctor: '[]'});
			case 'ToggleTabMenu':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$setShowingAnyMenuTrue(
						A3(_user$project$Update$toggleTabMenu, _p26._0, _p26._1, model)),
					{ctor: '[]'});
			case 'PinTabAtIndex':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$closeAllMenus(
						A2(
							_user$project$Update$startPinTabAnimation,
							A4(_user$project$Update$initPinningInfo, _p26._0, _p26._1, _p26._2, model),
							model)),
					{ctor: '[]'});
			case 'FinishPinningTab':
				var _p33 = _p26._0.oldTabIndex;
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					A2(_user$project$Update$pinTab, _p33, model),
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(
							A3(
								_elm_lang$core$Basics$clamp,
								_p33,
								_Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items) - 1,
								_p33 + 1)),
						_1: {ctor: '[]'}
					});
			case 'UnpinTabAtIndex':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$closeAllMenus(
						A2(
							_user$project$Update$startUnPinTabAnimation,
							A4(_user$project$Update$initUnPinningInfo, _p26._0, _p26._1, _p26._2, model),
							model)),
					{ctor: '[]'});
			case 'FinishUnpinningTab':
				var _p34 = _p26._0.oldTabIndex;
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					A3(_user$project$Update$unpinTab, _p34, _p26._0.newTabIndex, model),
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(
							A3(
								_elm_lang$core$Basics$clamp,
								_p34,
								_Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items) - 1,
								_p34 + 1)),
						_1: {ctor: '[]'}
					});
			case 'CloseTabAtIndex':
				var _p35 = _p26._0;
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$closeAllMenus(
						A2(_user$project$Update$closeTab, _p35, model)),
					(_elm_lang$core$Native_Utils.cmp(
						_Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items),
						1) > 0) ? {
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(
							A3(
								_elm_lang$core$Basics$clamp,
								0,
								_Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items) - 2,
								_p35)),
						_1: {ctor: '[]'}
					} : {ctor: '[]'});
			case 'CloseTabsOtherThanIndex':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$closeAllMenus(
						A2(_user$project$Update$closeTabsOtherThanIndex, _p26._0, model)),
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(0),
						_1: {ctor: '[]'}
					});
			case 'CloseTabsToTheRightOfIndex':
				var _p36 = _p26._0;
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$closeAllMenus(
						A2(_user$project$Update$closeTabsToTheRightOfIndex, _p36, model)),
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(_p36),
						_1: {ctor: '[]'}
					});
			case 'CloseAllMenus':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_user$project$Update$setShowingAnyMenuFalse(
						_user$project$Update$closeAllMenus(model)),
					{ctor: '[]'});
			case 'AddTab':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					A2(
						_user$project$Update$addTab,
						_user$project$Model$tabFromId(model.nextTabId),
						model),
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(0),
						_1: {ctor: '[]'}
					});
			case 'NewTabWidth':
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					_elm_lang$core$Native_Utils.update(
						model,
						{flexTabWidth: _p26._0}),
					{ctor: '[]'});
			default:
				return A2(
					_NoRedInk$rocket_update$Rocket_ops['=>'],
					model,
					{
						ctor: '::',
						_0: _user$project$Ports$getFlexTabWidth(
							_Skinney$elm_array_exploration$Array_Hamt$length(model.dragState.items) - 1),
						_1: {ctor: '[]'}
					});
		}
	});

var _user$project$View$viewLanguageInfo = function (tab) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('language-info'),
			_1: {ctor: '[]'}
		},
		function () {
			var _p0 = _user$project$Types$toLogo(tab.icon);
			switch (_p0.ctor) {
				case 'Elm':
					return {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$p,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Elm is a domain-specific programming language for declaratively creating web browser-based graphical user interfaces. Elm is purely functional, and is developed with emphasis on usability, performance, and robustness. It advertises \"no runtime exceptions in practice,\" made possible by the Elm compiler\'s static type checking.'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					};
				case 'Elixir':
					return {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$p,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Elixir is a functional, concurrent, general-purpose programming language that runs on the Erlang virtual machine (BEAM). Elixir builds on top of Erlang and shares the same abstractions for building distributed, fault-tolerant applications. Elixir also provides a productive tooling and an extensible design. The latter is supported by compile-time metaprogramming with macros and polymorphism via protocols.\n\nElixir is successfully used in the industry by companies such as Pinterest and Moz. Elixir is also used for web development, by companies such as Bleacher Report and Inverse, and for building embedded-systems. The community organizes yearly events in both United States and Europe as well as minor local events and conferences.'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					};
				default:
					return {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$p,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Haskell /ˈhæskəl/ is a standardized, general-purpose purely functional programming language, with non-strict semantics and strong static typing. It is named after logician Haskell Curry. The latest standard of Haskell is Haskell 2010. As of May 2016, a group is working on the next version, Haskell 2020.\n\nHaskell features a type system with type inference and lazy evaluation. Type classes first appeared in the Haskell programming language. Its main implementation is the Glasgow Haskell Compiler.\n\nHaskell is based on the semantics, but not the syntax, of the language Miranda, which served to focus the efforts of the initial Haskell working group. Haskell is used widely in academia and also used in industry.'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					};
			}
		}());
};
var _user$project$View$viewTabMenuItem = F4(
	function (tabRect, tab, pos, menuItem) {
		var menuItemBehaviors = function () {
			var _p1 = menuItem;
			switch (_p1.ctor) {
				case 'PinTab':
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseDown(
							A3(_user$project$Messages$PinTabAtIndex, _p1._0, tab, tabRect)),
						_1: {ctor: '[]'}
					};
				case 'UnpinTab':
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseDown(
							A3(_user$project$Messages$UnpinTabAtIndex, _p1._0, tab, tabRect)),
						_1: {ctor: '[]'}
					};
				case 'CloseTab':
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseDown(
							_user$project$Messages$CloseTabAtIndex(_p1._0)),
						_1: {ctor: '[]'}
					};
				case 'CloseOtherTabs':
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseDown(
							_user$project$Messages$CloseTabsOtherThanIndex(_p1._0)),
						_1: {ctor: '[]'}
					};
				default:
					return {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onMouseDown(
							_user$project$Messages$CloseTabsToTheRightOfIndex(_p1._0)),
						_1: {ctor: '[]'}
					};
			}
		}();
		return A2(
			_elm_lang$html$Html$li,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('tab-context-menu__item'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$button,
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('tab-context-menu__button'),
							_1: {ctor: '[]'}
						},
						menuItemBehaviors),
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_user$project$Types$tabMenuItemToString(menuItem)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _user$project$View$viewTabMenu = function (_p2) {
	var _p3 = _p2;
	var _p6 = _p3.tabIndex;
	var _p5 = _p3.tab;
	var _p4 = _p3.position;
	var menuItems = A2(
		_elm_lang$core$List$map,
		A3(_user$project$View$viewTabMenuItem, _p3.tabRect, _p5, _p4),
		A2(_user$project$Types$tabMenuItems, _p5.isPinned, _p6));
	var menuItemsWithDividers = _elm_lang$core$List$concat(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$List$take, 1, menuItems),
			_1: {
				ctor: '::',
				_0: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('tab-context-menu__divider'),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				},
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$List$drop, 1, menuItems),
					_1: {ctor: '[]'}
				}
			}
		});
	var px = function ($int) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString($int),
			'px');
	};
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$id('tab-menu'),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_lang$html$Html_Events$onWithOptions,
					'contextmenu',
					_user$project$Util$defaultPrevented,
					A2(
						_elm_lang$core$Json_Decode$map,
						_user$project$Messages$ToggleTabMenu(_p6),
						A3(
							_elm_lang$core$Json_Decode$map2,
							_user$project$Types$TabClickInfo,
							_elm_lang$mouse$Mouse$position,
							A2(_elm_lang$core$Json_Decode$field, 'current', _debois$elm_dom$DOM$boundingClientRect)))),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('tab-context-menu'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'top',
									_1: px(_p4.y)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'left',
										_1: px(_p4.x)
									},
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$ul,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('tab-context-menu__list'),
					_1: {ctor: '[]'}
				},
				menuItemsWithDividers),
			_1: {ctor: '[]'}
		});
};
var _user$project$View$viewCloseButton = function (index) {
	return A2(
		_elm_lang$html$Html$button,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('tab-close'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(
					_user$project$Messages$CloseTabAtIndex(index)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('accessible-hidden-text'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Close'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: _user$project$Logos$viewClose,
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$tabToLogo = function (tab) {
	var _p7 = _user$project$Types$toLogo(tab.icon);
	switch (_p7.ctor) {
		case 'Elm':
			return _user$project$Logos$viewElmLogo;
		case 'Elixir':
			return _user$project$Logos$viewElixirLogo;
		default:
			return _user$project$Logos$viewHaskellLogo;
	}
};
var _user$project$View$viewPlaceholderDetails = F3(
	function (isPinned, index, tab) {
		return {
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('tab-info'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('tab-logo'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _user$project$View$tabToLogo(tab),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: isPinned ? _elm_lang$html$Html$text('') : A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('tab-titles'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$h4,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('tab-title'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(tab.title),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$p,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('tab-subtitle'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('so good'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: _user$project$View$viewCloseButton(index),
				_1: {ctor: '[]'}
			}
		};
	});
var _user$project$View$viewPinningPlaceholder = F2(
	function (model, _p8) {
		var _p9 = _p8;
		var _p10 = _p9.tab;
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'top',
								_1: _user$project$Util$toPx(0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'left',
									_1: _user$project$Util$toPx(_p9.start.x)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'width',
										_1: _user$project$Util$toPx(_p9.startWidth)
									},
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('moving-tab'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$draggable('false'),
							_1: {ctor: '[]'}
						}
					}
				},
				_mdgriffith$elm_style_animation$Animation$render(model.pinPlaceholderStyle)),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'tab', _1: false},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'tab--selected',
										_1: _elm_lang$core$Native_Utils.eq(_p10.id, model.selected.id)
									},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'tab--pinned', _1: true},
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					},
					A3(_user$project$View$viewPlaceholderDetails, true, 0, _p10)),
				_1: {ctor: '[]'}
			});
	});
var _user$project$View$viewUnPinningPlaceholder = F2(
	function (model, _p11) {
		var _p12 = _p11;
		var _p13 = _p12.tab;
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'top',
								_1: _user$project$Util$toPx(0)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'left',
									_1: _user$project$Util$toPx(_p12.start.x)
								},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'width',
										_1: _user$project$Util$toPx(model.pinnedTabWidth)
									},
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('moving-tab'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$draggable('false'),
							_1: {ctor: '[]'}
						}
					}
				},
				_mdgriffith$elm_style_animation$Animation$render(model.pinPlaceholderStyle)),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'tab', _1: false},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'tab--selected',
										_1: _elm_lang$core$Native_Utils.eq(_p13.id, model.selected.id)
									},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'tab--pinned', _1: true},
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					},
					A3(_user$project$View$viewPlaceholderDetails, true, 0, _p13)),
				_1: {ctor: '[]'}
			});
	});
var _user$project$View$viewPinPlaceholder = F2(
	function (model, pinPlaceholder) {
		var _p14 = pinPlaceholder;
		if (_p14.ctor === 'Pinning') {
			return A2(_user$project$View$viewPinningPlaceholder, model, _p14._0);
		} else {
			return A2(_user$project$View$viewUnPinningPlaceholder, model, _p14._0);
		}
	});
var _user$project$View$viewTabReorderItem = F4(
	function (viewableReorderable, model, maybeDestIndex, index) {
		var _p15 = function () {
			var _p16 = viewableReorderable;
			if (_p16.ctor === 'NonPlaceholderReorderable') {
				var _p20 = _p16._0;
				var reorderItem = function () {
					var _p17 = model.pinPlaceholder;
					if (_p17.ctor === 'Just') {
						var _p18 = _p17._0;
						if (_p18.ctor === 'Pinning') {
							return _elm_lang$core$Native_Utils.eq(_p18._0.oldTabIndex, index) ? _user$project$Types$PinSourceBackdrop : (_elm_lang$core$Native_Utils.eq(_p18._0.newTabIndex, index) ? _user$project$Types$PinDestBackdrop : _user$project$Types$ReorderableTab);
						} else {
							return _elm_lang$core$Native_Utils.eq(_p18._0.oldTabIndex, index) ? _user$project$Types$UnPinSourceBackdrop : (_elm_lang$core$Native_Utils.eq(_p18._0.newTabIndex, index) ? _user$project$Types$PinDestBackdrop : _user$project$Types$ReorderableTab);
						}
					} else {
						return _elm_lang$core$Native_Utils.eq(
							maybeDestIndex,
							_elm_lang$core$Maybe$Just(index)) ? _user$project$Types$DropPreview : _user$project$Types$ReorderableTab;
					}
				}();
				var styles = _elm_lang$core$List$concat(
					{
						ctor: '::',
						_0: function () {
							var _p19 = reorderItem;
							if (_p19.ctor === 'DropPreview') {
								return {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'visibility', _1: 'hidden'},
									_1: {ctor: '[]'}
								};
							} else {
								return {ctor: '[]'};
							}
						}(),
						_1: {
							ctor: '::',
							_0: _p20.isPinned ? {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'width',
									_1: _user$project$Util$toPx(model.pinnedTabWidth)
								},
								_1: {ctor: '[]'}
							} : {ctor: '[]'},
							_1: {ctor: '[]'}
						}
					});
				return {
					ctor: '_Tuple4',
					_0: _p20,
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$href(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'#',
								_elm_lang$core$Basics$toString(_p20.id))),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('draggable tab-invisible-link'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$classList(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'tab', _1: !_p20.isPinned},
										_1: {
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'tab--selected',
												_1: _elm_lang$core$Native_Utils.eq(model.selected.id, _p20.id)
											},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'tab--pinned', _1: _p20.isPinned},
												_1: {
													ctor: '::',
													_0: {
														ctor: '_Tuple2',
														_0: A2(
															_elm_lang$core$Basics_ops['++'],
															'tab-id-',
															_elm_lang$core$Basics$toString(_p20.id)),
														_1: true
													},
													_1: {ctor: '[]'}
												}
											}
										}
									}),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(styles),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html_Attributes$attribute,
											'data-reorderable-index',
											_elm_lang$core$Basics$toString(index)),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$contextmenu('tab-menu'),
											_1: {
												ctor: '::',
												_0: A3(
													_elm_lang$html$Html_Events$onWithOptions,
													'contextmenu',
													_user$project$Util$defaultPrevented,
													A2(
														_elm_lang$core$Json_Decode$map,
														_user$project$Messages$ToggleTabMenu(index),
														A3(
															_elm_lang$core$Json_Decode$map2,
															_user$project$Types$TabClickInfo,
															_elm_lang$mouse$Mouse$position,
															A2(_elm_lang$core$Json_Decode$field, 'currentTarget', _debois$elm_dom$DOM$boundingClientRect)))),
												_1: {
													ctor: '::',
													_0: _elm_lang$html$Html_Events$onClick(
														_user$project$Messages$SetActive(_p20)),
													_1: {
														ctor: '::',
														_0: _elm_lang$html$Html_Events$onMouseDown(_user$project$Messages$CloseAllMenus),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					},
					_2: _elm_lang$core$Native_Utils.eq(model.selected.id, _p20.id),
					_3: reorderItem
				};
			} else {
				var _p22 = _p16._1;
				var _p21 = _p16._0;
				var styles = A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'position', _1: 'fixed'},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'left',
								_1: _user$project$Util$toPx(_p22.x)
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: 'top',
									_1: _user$project$Util$toPx(_p22.y)
								},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'margin', _1: '0'},
									_1: {ctor: '[]'}
								}
							}
						}
					},
					_p21.isPinned ? {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'width',
							_1: _user$project$Util$toPx(model.pinnedTabWidth)
						},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'box-sizing', _1: 'border-box'},
							_1: {ctor: '[]'}
						}
					} : {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'width',
							_1: _user$project$Util$toPx(model.flexTabWidth)
						},
						_1: {ctor: '[]'}
					});
				return {
					ctor: '_Tuple4',
					_0: _p21,
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('draggable tab-invisible-link'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(styles),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$classList(
										{
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'tab', _1: !_p21.isPinned},
											_1: {
												ctor: '::',
												_0: {
													ctor: '_Tuple2',
													_0: 'tab--selected',
													_1: _elm_lang$core$Native_Utils.eq(model.selected.id, _p21.id)
												},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'tab--pinned', _1: _p21.isPinned},
													_1: {ctor: '[]'}
												}
											}
										}),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$id('reorderable-placeholder'),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						_mdgriffith$elm_style_animation$Animation$render(model.placeholderAnimationStyle)),
					_2: _elm_lang$core$Native_Utils.eq(model.selected.id, _p21.id),
					_3: _user$project$Types$ReorderableTab
				};
			}
		}();
		var tab = _p15._0;
		var attrs = _p15._1;
		var isSelected = _p15._2;
		var reorderItem = _p15._3;
		var viewTab = A2(
			_elm_lang$html$Html$a,
			attrs,
			A3(_user$project$View$viewPlaceholderDetails, tab.isPinned, index, tab));
		var _p23 = reorderItem;
		switch (_p23.ctor) {
			case 'ReorderableTab':
				return {
					ctor: '::',
					_0: viewTab,
					_1: {ctor: '[]'}
				};
			case 'DropPreview':
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('drop-preview'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(
									{
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'width',
											_1: tab.isPinned ? _user$project$Util$toPx(model.pinnedTabWidth) : _user$project$Util$toPx(model.flexTabWidth)
										},
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: viewTab,
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			case 'PinSourceBackdrop':
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('pin-drop-preview'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(
										{
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'width',
												_1: _user$project$Util$toPx(model.flexTabWidth)
											},
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							},
							_mdgriffith$elm_style_animation$Animation$render(model.pinStartBackdropStyle)),
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				};
			case 'UnPinSourceBackdrop':
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('pin-drop-preview'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(
										{
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'width',
												_1: _user$project$Util$toPx(model.pinnedTabWidth)
											},
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							},
							_mdgriffith$elm_style_animation$Animation$render(model.pinStartBackdropStyle)),
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				};
			default:
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('pin-drop-preview'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(
										{
											ctor: '::',
											_0: {
												ctor: '_Tuple2',
												_0: 'width',
												_1: _user$project$Util$toPx(0)
											},
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							},
							_mdgriffith$elm_style_animation$Animation$render(model.pinDestinationStyle)),
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: viewTab,
						_1: {ctor: '[]'}
					}
				};
		}
	});
var _user$project$View$viewTooltipMask = function (showingAnyMenu) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'root-app', _1: true},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'hidden', _1: !showingAnyMenu},
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(_user$project$Messages$CloseAllMenus),
				_1: {ctor: '[]'}
			}
		},
		{ctor: '[]'});
};
var _user$project$View$viewTooltips = function (model) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_elm_lang$html$Html$text(''),
		A2(_elm_lang$core$Maybe$map, _user$project$View$viewTabMenu, model.tabMenu));
};
var _user$project$View$viewNonPlaceholderTab = F4(
	function (model, maybeDestIndex, index, tab) {
		return A4(
			_user$project$View$viewTabReorderItem,
			_user$project$Reorderable_State$NonPlaceholderReorderable(tab),
			model,
			maybeDestIndex,
			index);
	});
var _user$project$View$viewTabs = F3(
	function (model, maybeDestIndex, tabs) {
		return _elm_lang$core$List$concat(
			_Skinney$elm_array_exploration$Array_Hamt$toList(
				A2(
					_Skinney$elm_array_exploration$Array_Hamt$indexedMap,
					A2(_user$project$View$viewNonPlaceholderTab, model, maybeDestIndex),
					tabs)));
	});
var _user$project$View$viewAddButton = A2(
	_elm_lang$html$Html$button,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('add-tab'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Events$onClick(_user$project$Messages$AddTab),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: _elm_lang$html$Html$text('Add +'),
		_1: {ctor: '[]'}
	});
var _user$project$View$viewTabsAndAddButton = F3(
	function (model, maybeDestIndex, tabs) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('tab-list'),
				_1: {ctor: '[]'}
			},
			A3(
				_elm_lang$core$Basics$flip,
				_elm_lang$core$List$append,
				{
					ctor: '::',
					_0: _user$project$View$viewAddButton,
					_1: {ctor: '[]'}
				},
				A3(_user$project$View$viewTabs, model, maybeDestIndex, tabs)));
	});
var _user$project$View$view = function (model) {
	var _p24 = function () {
		var _p25 = model.dragState.placeholder;
		if (_p25.ctor === 'Just') {
			var _p26 = _p25._0.destIndex;
			return {
				ctor: '_Tuple2',
				_0: A4(
					_user$project$View$viewTabReorderItem,
					A2(_user$project$Reorderable_State$PlaceholderReorderable, _p25._0.draggable, _p25._0.point),
					model,
					_p26,
					_p25._0.sourceIndex),
				_1: _p26
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: {
					ctor: '::',
					_0: _elm_lang$html$Html$text(''),
					_1: {ctor: '[]'}
				},
				_1: _elm_lang$core$Maybe$Nothing
			};
		}
	}();
	var placeholder = _p24._0;
	var maybeDestIndex = _p24._1;
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('tabs-app'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _user$project$View$viewTooltips(model),
			_1: {
				ctor: '::',
				_0: _user$project$View$viewTooltipMask(model.showingAnyMenu),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('tabs-container'),
							_1: {ctor: '[]'}
						},
						A2(
							_elm_lang$core$Basics_ops['++'],
							{
								ctor: '::',
								_0: A3(_user$project$View$viewTabsAndAddButton, model, maybeDestIndex, model.dragState.reorderedItems),
								_1: {
									ctor: '::',
									_0: _user$project$View$viewLanguageInfo(model.selected),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$core$Maybe$withDefault,
											_elm_lang$html$Html$text(''),
											A2(
												_elm_lang$core$Maybe$map,
												_user$project$View$viewPinPlaceholder(model),
												model.pinPlaceholder)),
										_1: {ctor: '[]'}
									}
								}
							},
							placeholder)),
					_1: {ctor: '[]'}
				}
			}
		});
};

var _user$project$Main$main = _elm_lang$html$Html$program(
	{
		init: _NoRedInk$rocket_update$Rocket$batchInit(
			{
				ctor: '_Tuple2',
				_0: _user$project$Model$initialModel,
				_1: {
					ctor: '::',
					_0: _user$project$Ports$getFlexTabWidth(0),
					_1: {ctor: '[]'}
				}
			}),
		update: function (_p0) {
			return _NoRedInk$rocket_update$Rocket$batchUpdate(
				_user$project$Update$update(_p0));
		},
		view: _user$project$View$view,
		subscriptions: _user$project$Subscriptions$subscriptions
	})();

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', {"types":{"unions":{"Animation.Model.Tick":{"args":[],"tags":{"Tick":["Time.Time"]}},"Messages.Msg":{"args":[],"tags":{"CloseTabsToTheRightOfIndex":["Int"],"CloseAllMenus":[],"CloseTabAtIndex":["Int"],"FinishPinningTab":["Types.PinningPlaceholder"],"NewTabWidth":["Float"],"AnimateMessenger":["Animation.Msg"],"UnpinTabAtIndex":["Int","Types.Tab","DOM.Rectangle"],"PinTabAtIndex":["Int","Types.Tab","DOM.Rectangle"],"ToggleTabMenu":["Int","Types.TabClickInfo"],"SetActive":["Types.Tab"],"KeyboardExtraMsg":["Keyboard.Extra.Msg"],"FinishUnpinningTab":["Types.UnPinningPlaceholder"],"CloseTabsOtherThanIndex":["Int"],"FinishSlidingTab":["Types.SlidingPlaceholder"],"WindowResize":["Window.Size"],"AddTab":[],"DragMsg":["Reorderable.Update.DragMsg"]}},"Keyboard.Extra.Msg":{"args":[],"tags":{"Down":["Keyboard.KeyCode"],"Up":["Keyboard.KeyCode"]}},"Reorderable.Update.DragMsg":{"args":[],"tags":{"DragHold":["{ placeholder : { point : Reorderable.State.Point , bounds : Reorderable.State.Bounds } , reorderableBounds : List Reorderable.State.Bounds }"],"DragStop":[],"DragMove":["{ clientSize : Reorderable.State.Size , cursor : Reorderable.State.Point , placeholder : { point : Reorderable.State.Point , bounds : Reorderable.State.Bounds } , reorderableBounds : List Reorderable.State.Bounds }"],"DragStart":["{ sourceIndex : Int, point : Reorderable.State.Point }"]}}},"aliases":{"Types.SlidingPlaceholder":{"args":[],"type":"{ start : Reorderable.State.Point , end : Reorderable.State.Point , sourceTabIndex : Int , destTabIndex : Int , tab : Types.Tab }"},"Types.PinningPlaceholder":{"args":[],"type":"{ start : Reorderable.State.Point , end : Reorderable.State.Point , startWidth : Float , oldTabIndex : Int , newTabIndex : Int , tab : Types.Tab }"},"Types.TabClickInfo":{"args":[],"type":"{ position : Mouse.Position, rect : DOM.Rectangle }"},"Types.Tab":{"args":[],"type":"{ id : Int, title : String, icon : String, isPinned : Bool }"},"Mouse.Position":{"args":[],"type":"{ x : Int, y : Int }"},"Reorderable.State.Size":{"args":[],"type":"{ height : Float, width : Float }"},"Keyboard.KeyCode":{"args":[],"type":"Int"},"Types.UnPinningPlaceholder":{"args":[],"type":"{ start : Reorderable.State.Point , end : Reorderable.State.Point , endWidth : Float , oldTabIndex : Int , newTabIndex : Int , tab : Types.Tab }"},"Window.Size":{"args":[],"type":"{ width : Int, height : Int }"},"Reorderable.State.Bounds":{"args":[],"type":"{ top : Float, left : Float, bottom : Float, right : Float }"},"Reorderable.State.Point":{"args":[],"type":"{ x : Float, y : Float }"},"Time.Time":{"args":[],"type":"Float"},"DOM.Rectangle":{"args":[],"type":"{ top : Float, left : Float, width : Float, height : Float }"},"Animation.Msg":{"args":[],"type":"Animation.Model.Tick"}},"message":"Messages.Msg"},"versions":{"elm":"0.18.0"}});
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

