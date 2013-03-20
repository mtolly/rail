/**
 * @constructor
 */
function Pair(car, cdr) {
  this.car = car;
  this.cdr = cdr;
}

var stack = [];
var size = 0;

function error(str) {
  throw new Error(str);
}

// accepts numbers, bools, strings, Pairs, or null
function push(x) {
  var t = typeof x;
  if (t == "number") {
    stack.push(x.toString());
  }
  else if (t == "boolean") {
    stack.push(x ? '1' : '0');
  }
  else {
    stack.push(x);
  }
  size++;
}

function pop() {
  if (size == 0) {
    error("pop: empty stack");
  }
  size--;
  return stack.pop();
}

function pop_str() {
  x = pop();
  if (typeof x != 'string') {
    error("pop_str: expected string");
  }
  return x;
}

function pop_int() {
  x = pop_str().parseInt();
  if (isNaN(x)) {
    error("pop_int: expected int");
  }
  return x;
}

function pop_bool() {
  x = pop_str();
  if (x == '1') {
    return true;
  }
  if (x == '0') {
    return false;
  }
  error("pop_bool: expected '0' or '1'");
}

function trampoline(f) {
  while (f !== null) {
    f = f();
  }
}

function builtin_boom() {
  error(pop());
}

function builtin_underflow() {
  push(size);
}

function builtin_type() {
  var x = pop();
  if (typeof x == 'string') {
    push("string");
  }
  else if (x instanceof Pair) {
    push("list");
  }
  else if (x === null) {
    push("nil");
  }
  else {
    error("fatal: unknown type");
  }
}

var input = "foo\nbar\nbaz";
function builtin_input() {
  if (input.length == 0) {
    error('input: end of file');
  }
  push(input.substring(0, 1));
  input = input.substring(1, input.length);
}

function builtin_output() {
  process.stdout.write(pop_str());
}

function builtin_eof() {
  push(input.length == 0);
}

function builtin_add() {
  push(pop_int() + pop_int());
}

function builtin_div() {
  var y = pop_int();
  var x = pop_int();
  push(~~(x / y));
}

function builtin_mult() {
  push(pop_int() * pop_int());
}

function builtin_rem() {
  var y = pop_int();
  var x = pop_int();
  push(x % y);
}

function builtin_sub() {
  var y = pop_int();
  var x = pop_int();
  push(x - y);
}

function builtin_cut() {
  var s = pop_str();
  var i = pop_int();
  var len = s.length;
  if (i < 0 || len < i) {
    error("cut: index out of bounds");
  }
  push(s.substring(0, i));
  push(s.substring(i, len));
}

function builtin_append() {
  var y = pop_str();
  var x = pop_str();
  push(x + y);
}

function builtin_size() {
  push(pop_str().length);
}

function builtin_cons() {
  var y = pop();
  var x = pop();
  push(new Pair(x, y));
}

function builtin_uncons() {
  var x = pop();
  if (x instanceof Pair) {
    push(x.car);
    push(x.cdr);
  }
  else {
    error("uncons: expected pair");
  }
}

function builtin_greater() {
  push(pop_int() < pop_int()); // < because pop order
}

function builtin_equal() {
  push(pop() === pop());
}

/*
// cat example
function fun_main() {
  var start = function () {
    return path_E_5_4;
  };
  var path_E_5_4 = function () {
    builtin_eof();
    if (pop_bool()) {
      return null;
    } else {
      builtin_input();
      builtin_output();
      return path_E_5_4;
    }
  };
  trampoline(start);
}
*/

/*
// dup example
function fun_dup() {
  var var_a;
  var start = function () {
    var_a = pop();
    push(var_a);
    push(var_a);
    return null;
  };
  trampoline(start);
}

function fun_main() {
  var start = function () {
    push(5);
    fun_dup();
    builtin_output();
    builtin_output();
    return null;
  }
  trampoline(start);
}
*/

fun_main();
process.exit();


