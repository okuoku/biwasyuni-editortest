// biwascore: BiwaScheme fork for app embedding 
//
// See LICENSE.biwascheme for the original license terms

var _ = require("underscore");
var _str = require("underscore.string");

_.str = _str;

/*
 * BiwaScheme @VERSION@ - R6RS Scheme in JavaScript
 *
 * Copyright (c) 2007-2014 Yutaka HARA (http://www.biwascheme.org/)
 * @license Licensed under the MIT license.
 */

var BiwaScheme = {};

BiwaScheme.VERSION = "0.6.8.yuni";
BiwaScheme.GitCommit = "none";
// 
// Heap based scheme from 3imp.pdf
//

//
// variables
//
BiwaScheme.TopEnv = {};
BiwaScheme.CoreEnv = {};

//
// Nil
// javascript representation of empty list( '() )
//
BiwaScheme.nil = {
  toString: function() { return "nil"; },
  to_write: function() { return "()"; },
  to_array: function() { return []; },
  length: function() { return 0; }
};

//
// #<undef> (The undefined value)
// also used as #<unspecified> values
//
BiwaScheme.undef = new Object();
BiwaScheme.undef.toString = function(){ return "#<undef>"; }

// Prints the arguments to console.debug.
BiwaScheme.debug = function(/*arguments*/){
  var args = _.toArray(arguments);
  console.debug.apply(console, _.map(args, BiwaScheme.inspect));
}

//
// Assertion
//
BiwaScheme.assert = function(cond, desc) {
  if (!cond) {
    throw new BiwaScheme.Bug("[BUG] Assertion failed: "+desc);
  }
}

//
// Configurations
//

// Maximum depth of stack trace
// (You can also set Interpreter#max_trace_size for each Interpreter)
BiwaScheme.max_trace_size = 40;

// Stop showing deprecation warning
BiwaScheme.suppress_deprecation_warning = false;
//
// Super-simple class implementation
//
// Example usage:
//
// BiwaScheme.Foo = BiwaScheme.Class.create({
//   initialize: function(a){
//     this.a = a;
//   },
//
//   toString: function(){
//     return "foo[" + this.a + "]";
//   }
// });
//
// BiwaScheme.Bar = BiwaScheme.Class.extend(new BiwaScheme.Foo("hello1"), {
//   initialize: function(b){
//     this.b = b;
//   },
//
//   printEverything: function(){
//     console.log("a = ", this.a, "b = ", this.b);
//   },
//
//   toString: function(){
//     return "bar[" + this.b + "]";
//   }
// });

BiwaScheme.Class = {
  create: function(methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    _.extend(klass.prototype, methods);
    return klass;
  },

  extend: function(parent, methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    klass.prototype = parent;
    _.extend(klass.prototype, methods);
    return klass;
  }
};

// Update the given method to memoized version.
//
// - klass : a class defined by BiwaScheme.Class.create
// - name_or_names : method name (a string or an array of strings)
//
// Example
//
//   // Given this method
//   BiwaScheme.Enumeration.EnumType = ...
//     universe: function(){
//       return ...
//     }
//   ...
//   // Memoize
//   BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumType,
//                            "_universe"); 
//
//   // Equal to:
//   BiwaScheme.Enumeration.EnumType = ...
//     universe: function(){
//       if(!this.hasOwnProperty("cached_universe")){
//         this.cached_universe = this.compute_universe();
//       }
//       return this.cached_universe;
//     },
//     compute_universe: function(){ 
//       // Original function, renamed to compute_*
//       return ...
//     }
//   ...
BiwaScheme.Class.memoize = function(klass, name_or_names){
  var proto = klass.prototype;
  var names = _.isArray(name_or_names) ? name_or_names : [name_or_names];

  _.each(names, function(name){
    // Copy original function foo as 'compute_foo'
    proto["compute_"+name] = proto[name];

    // Define memoizing version
    proto[name] = function(/*arguments*/){
      if(!this.hasOwnProperty("cached_"+name)){
        this["cached_"+name] = this["compute_"+name].apply(this, _.toArray(arguments));
      }
      return this["cached_"+name];
    }
  });
}
//
// write.js: Functions to convert objects to strings
//

//
// write
//

BiwaScheme.to_write = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(_.isFunction(obj))
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? _.str.truncate(obj.toSource(), 40) :
                          "")+">";
  else if(_.isString(obj))
    return '"' +
           obj.replace(/\\|\"/g,function($0){return'\\'+$0;})
              .replace(/\x07/g, "\\a")
              .replace(/\x08/g, "\\b")
              .replace(/\t/g, "\\t")
              .replace(/\n/g, "\\n")
              .replace(/\v/g, "\\v")
              .replace(/\f/g, "\\f")
              .replace(/\r/g, "\\r") +
           '"';
  else if(_.isArray(obj) && obj.closure_p)
    return "#<Closure>";
  else if(_.isArray(obj))
    return "#(" + _.map(obj, function(e) { return BiwaScheme.to_write(e); }).join(" ") + ")";
  else if(typeof(obj.to_write) == 'function')
    return obj.to_write();
  else if(isNaN(obj) && typeof(obj) == 'number')
    return "+nan.0";
  else{
    switch(obj){
      case true: return "#t";
      case false: return "#f";
      case Infinity: return "+inf.0";
      case -Infinity: return "-inf.0";
    }
  }
  return BiwaScheme.inspect(obj);
}

//
// display
//

BiwaScheme.to_display = function(obj){
  if(_.isUndefined(obj))
    return 'undefined';
  else if(_.isNull(obj))
    return 'null';
  else if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaScheme.Symbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + _.map(obj, BiwaScheme.to_display).join(' ') + ')';
  else if(obj instanceof BiwaScheme.Pair)
    return obj.inspect(BiwaScheme.to_display);
  else if(obj instanceof BiwaScheme.Char)
    return obj.value;
  else
    return BiwaScheme.to_write(obj);
}

//
// write/ss (write with substructure)
//

// example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
// 2-pass algorithm.
// (1) detect all the objects which appears more than once
//     (find_cyclic, reduce_cyclic_info)
// (2) write object using this information
//   * add prefix '#n=' for first appearance
//   * just write '#n#' for other appearance

//TODO: support Values
BiwaScheme.write_ss = function(obj, array_mode){
  var known = [obj], used = [false];
  BiwaScheme.find_cyclic(obj, known, used);
  var cyclic   = BiwaScheme.reduce_cyclic_info(known, used);
  var appeared = new Array(cyclic.length);
  for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

  return BiwaScheme.to_write_ss(obj, cyclic, appeared, array_mode);
}
BiwaScheme.to_write_ss = function(obj, cyclic, appeared, array_mode){
  var ret = "";
  var i = cyclic.indexOf(obj);
  if(i >= 0){
    if(appeared[i]){
      return "#"+i+"#";
    }
    else{
      appeared[i] = true;
      ret = "#"+i+"=";
    }
  }

  if(obj instanceof BiwaScheme.Pair){
    var a = [];
    a.push(BiwaScheme.to_write_ss(obj.car, cyclic, appeared, array_mode));
    for(var o=obj.cdr; o != BiwaScheme.nil; o=o.cdr){
      if(!(o instanceof BiwaScheme.Pair) || cyclic.indexOf(o) >= 0){
        a.push(".");
        a.push(BiwaScheme.to_write_ss(o, cyclic, appeared, array_mode));
        break;
      }
      a.push(BiwaScheme.to_write_ss(o.car, cyclic, appeared, array_mode));
    }
    ret += "(" + a.join(" ") + ")";
  }
  else if(obj instanceof Array){
    var a = _.map(obj, function(item){
      return BiwaScheme.to_write_ss(item, cyclic, appeared, array_mode);
    })
    if(array_mode)
      ret += "[" + a.join(", ") + "]";
    else
      ret += "#(" + a.join(" ") + ")";
  }
  else{
    ret += BiwaScheme.to_write(obj);
  }
  return ret;
}
BiwaScheme.reduce_cyclic_info = function(known, used){
  var n_used = 0;
  for(var i=0; i<used.length; i++){
    if(used[i]){
      known[n_used] = known[i];
      n_used++;
    }
  }
  return known.slice(0, n_used);
}
BiwaScheme.find_cyclic = function(obj, known, used){
  var items = (obj instanceof BiwaScheme.Pair)  ? [obj.car, obj.cdr] :
              (obj instanceof Array) ? obj :
              null;
  if(!items) return;

  _.each(items, function(item){
    if(typeof(item)=='number' || typeof(item)=='string' ||
      item === BiwaScheme.undef || item === true || item === false ||
      item === BiwaScheme.nil || item instanceof BiwaScheme.Symbol) return;

    var i = known.indexOf(item);
    if(i >= 0)
      used[i] = true;
    else{
      known.push(item);
      used.push(false);
      BiwaScheme.find_cyclic(item, known, used);
    }
  });
};

//
// inspect
//
BiwaScheme.inspect = function(object, opts) {
  try {
    if (_.isUndefined(object)) return 'undefined';
    if (object === null) return 'null';
    if (object === true) return '#t';
    if (object === false) return '#f';
    if (object.inspect) return object.inspect();
    if (_.isString(object)) {
      return '"' + object.replace(/"/g, '\\"') + '"';
    }
    if (_.isArray(object)) {
      return '[' + _.map(object, BiwaScheme.inspect).join(', ') + ']';
    }

    if (opts && opts["fallback"]){
      return opts["fallback"];
    }
    else {
      return object.toString();
    }
  } catch (e) {
    if (e instanceof RangeError) return '...';
    throw e;
  }
};
//
// types.js - type predicators, equality, compare
//

BiwaScheme.isNil = function(obj){
  return (obj === BiwaScheme.nil);
};

BiwaScheme.isUndef = function(obj){
  return (obj === BiwaScheme.undef);
};

BiwaScheme.isBoolean = _.isBoolean; // Return true if arg is either true or false

//BiwaScheme.isNumber is defined in number.js (Return true if arg is scheme number)

BiwaScheme.isString = _.isString;

BiwaScheme.isChar = function(obj){
  return (obj instanceof BiwaScheme.Char);
};

BiwaScheme.isSymbol = function(obj){
  return (obj instanceof BiwaScheme.Symbol);
};

BiwaScheme.isPort = function(obj){
  return (obj instanceof BiwaScheme.Port);
};

// Note: '() is not a pair in scheme
BiwaScheme.isPair = function(obj){
  return (obj instanceof BiwaScheme.Pair);
};

// Returns true if obj is a proper list
// Note: isList returns true for '()
BiwaScheme.isList = function(obj){
  var nil = BiwaScheme.nil, Pair = BiwaScheme.Pair;

  if (obj === nil) { // Empty list
    return true;
  }
  if (!(obj instanceof Pair)) { // Argument isn't even a pair
    return false;
  }

  var tortoise = obj;
  var hare = obj.cdr;
  while (true) {
    if (hare === nil) { // End of list
      return true;
    }
    if (hare === tortoise) { // Cycle
      return false;
    }
    if (!(hare instanceof Pair)) { // Improper list
      return false;
    }

    if (hare.cdr === nil) { // End of list
      return true;
    }
    if (!(hare.cdr instanceof Pair)) { // Improper list
      return false;
    }

    hare = hare.cdr.cdr;
    tortoise = tortoise.cdr;
  }
};

BiwaScheme.isVector = function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
};

BiwaScheme.isHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable);
};

BiwaScheme.isMutableHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.mutable;
};

BiwaScheme.isClosure = function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
BiwaScheme.isProcedure = function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
};

// Return true if obj is a scheme value which evaluates to itself
BiwaScheme.isSelfEvaluating = function(obj) {
  return BiwaScheme.isBoolean(obj) ||
         BiwaScheme.isNumber(obj) ||
         BiwaScheme.isString(obj) ||
         BiwaScheme.isChar(obj);
};

//
// equality
//
BiwaScheme.eq = function(a, b){
  return a === b;
};
// TODO: Records (etc.)
BiwaScheme.eqv = function(a, b){
  return a == b && (typeof(a) == typeof(b));
};
BiwaScheme.equal = function(a, b){
  //TODO: must terminate for cyclic objects
  return BiwaScheme.to_write(a) == BiwaScheme.to_write(b);
};

//
// comaprator
//
// Return true when a < b
BiwaScheme.lt = function(a, b) {
  if(typeof a !== typeof b){
    return compareFn(typeof a, typeof b); 	
  }
  return a < b;
};
//
// Errors
//

BiwaScheme.Error = BiwaScheme.Class.create({
  initialize: function(msg){
    this.message = "Error: "+msg;
  },
  toString: function(){
    return this.message;
  }
});

BiwaScheme.Bug = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = "[BUG] "+msg;
  }
});

// currently used by "raise"
BiwaScheme.UserError = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = msg;
  }
});

//
// Set - set of string
// contents must be string (or at least sortable)
//
BiwaScheme.Set = BiwaScheme.Class.create({
  initialize : function(/*args*/){
    this.arr = [];
    var i;
    for(i=0; i<arguments.length; i++)
      this.arr[i] = arguments[i];
  },

  equals : function(other){
    if(this.arr.length != other.arr.length)
      return false;

    var a1 = _.clone(this.arr);
    var a2 = _.clone(other.arr);
    a1.sort();
    a2.sort();
    for(var i=0; i<this.arr.length; i++){
      if(a1[i] != a2[i]) return false;
    }
    return true;
  },
  set_cons : function(item){
    var o = new BiwaScheme.Set(item);
    o.arr = _.clone(this.arr);
    o.arr.push(item);
    return o;
  },
  set_union : function(/*args*/){
    var o = new BiwaScheme.Set();
    o.arr = _.clone(this.arr);

    for(var k=0; k<arguments.length; k++){
      var s2 = arguments[k];
      if(!(s2 instanceof BiwaScheme.Set))
        throw new BiwaScheme.Error("set_union: arguments must be a set");

      for(var i=0; i<s2.arr.length; i++)
        o.add(s2.arr[i]);
    }
    return o;
  },
  set_intersect : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_intersect: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  set_minus : function(s2){
    if(!(s2 instanceof BiwaScheme.Set))
      throw new BiwaScheme.Error("set_minus: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(!s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  add : function(item){
    if(!this.member(item)){
      this.arr.push(item);
    }
  },
  member : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return true;

    return false;
  },
  rindex : function(item){
    for(var i=this.arr.length-1; i>=0 ; i--)
      if(this.arr[i] == item) return (this.arr.length-1-i);

    return null;
  },
  index : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return i;

    return null;
  },
  inspect : function(){
    return "Set(" + this.arr.join(", ") + ")";
  },
  toString : function(){
    return this.inspect();
  },
  size : function(){
    return this.arr.length;
  }
});
//
// Values
//
BiwaScheme.Values = BiwaScheme.Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             _.map(this.content, BiwaScheme.to_write).join(" ") +
           ">";
  }
});

//
// Pair 
// cons cell
//

BiwaScheme.Pair = BiwaScheme.Class.create({
  initialize: function(car, cdr){
    this.car = car;
    this.cdr = cdr;
  },

  caar: function(){ return this.car.car; },
  cadr: function(){ return this.cdr.car; },
  cdar: function(){ return this.cdr.car; },
  cddr: function(){ return this.cdr.cdr; },

  first:  function(){ return this.car; },
  second: function(){ return this.cdr.car; },
  third:  function(){ return this.cdr.cdr.car; },
  fourth: function(){ return this.cdr.cdr.cdr.car; },
  fifth:  function(){ return this.cdr.cdr.cdr.cdr.car; },

  // returns array containing all the car's of list
  // '(1 2 3) => [1,2,3]
  // '(1 2 . 3) => [1,2]
  to_array: function(){
    var ary = [];
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  },

  to_set: function(){
    var set = new BiwaScheme.Set();
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  },

  length: function(){
    var n = 0;
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      n++;
    }
    return n;
  },

  // Return the last cdr
  last_cdr: function(){
    var o;
    for(o = this; o instanceof BiwaScheme.Pair; o = o.cdr)
      ;
    return o;
  },

  // calls the given func passing each car of list
  // returns cdr of last Pair
  foreach: function(func){
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  },

  // Returns an array which contains the resuls of calling func
  // with the car's as an argument.
  // If the receiver is not a proper list, the last cdr is ignored.
  // The receiver must not be a cyclic list.
  map: function(func){
    var ary = [];
    for(var o = this; BiwaScheme.isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  },

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat: function(list){
    var o = this;
    while(o instanceof BiwaScheme.Pair && o.cdr != BiwaScheme.nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  },

  // returns human-redable string of pair
  inspect: function(conv){
    conv || (conv = BiwaScheme.inspect);
    var a = [];
    var last = this.foreach(function(o){
      a.push(conv(o));
    });
    if(last != BiwaScheme.nil){
      a.push(".");
      a.push(conv(last));
    }
    return "(" + a.join(" ") + ")";
  },
  toString : function(){
    return this.inspect();
  },

  to_write: function(){
    return this.inspect(BiwaScheme.to_write);
  }
});

// Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
//   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
var array_to_list = function(ary, deep) {
  var list = BiwaScheme.nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && _.isArray(obj) && !obj.is_vector){
      obj = array_to_list(obj, deep);
    }
    list = new BiwaScheme.Pair(obj, list);
  }
  return list;
}

// Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.List = function() {
  var ary = _.toArray(arguments);
  return array_to_list(ary, false);
};

// Shallow: array_to_list(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.array_to_list = function(ary) {
  return array_to_list(ary, false);
};

// Deep: deep_array_to_list(1, 2, [3, 4]) == (list 1 2 (list 3 4))
// deep_array_to_list([1, 2, 3]) - deep
BiwaScheme.deep_array_to_list = function(ary) {
  return array_to_list(ary, true);
};

BiwaScheme.Cons = function(car, cdr) {
  return new BiwaScheme.Pair(car, cdr);
};
//
// Symbol
//

BiwaScheme.Symbol = BiwaScheme.Class.create({
  initialize: function(str){
    this.name = str;
    BiwaScheme.Symbols[ str ] = this;
  },

  inspect: function(){
    return "'"+this.name;
    //return "#<Symbol '"+this.name+"'>";
  },

  toString: function(){
    return "'"+this.name;
  },

  to_write: function(){
    return this.name;
  }
});
BiwaScheme.Symbols = {};
BiwaScheme.Sym = function(name,leaveCase){
  if( BiwaScheme.Symbols[name] === undefined ){
    return new BiwaScheme.Symbol(name);
  }
  else if( ! (BiwaScheme.Symbols[name] instanceof BiwaScheme.Symbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new BiwaScheme.Symbol(name);
  }
  else{
    return BiwaScheme.Symbols[name];
  }
}

BiwaScheme.gensym = function(){
  return BiwaScheme.Sym(_.uniqueId("__gensym"));
};
//
// Char
//

BiwaScheme.Char = BiwaScheme.Class.create({
  initialize: function(c){
    BiwaScheme.Chars[ this.value = c ] = this;
  },
  to_write: function(){
    switch(this.value){
      case '\n': return "#\\newline";
      case ' ':  return "#\\space";
      case '\t': return "#\\tab";
      default:   return "#\\"+this.value;
    }
  },
  inspect: function(){
    return this.to_write();
  }
});
BiwaScheme.Chars = {};
BiwaScheme.Char.get = function(c) {
  if(typeof(c) != "string") {
    throw new BiwaScheme.Bug("Char.get: " +
                             BiwaScheme.inspect(c) + " is not a string");
  }
  if( BiwaScheme.Chars[c] === undefined )
    return new BiwaScheme.Char(c);
  else
    return BiwaScheme.Chars[c];
};

//
// number.js
//

//
// Complex
//
BiwaScheme.Complex = BiwaScheme.Class.create({
  initialize: function(real, imag){
    this.real = real;
    this.imag = imag;
  },
  magnitude: function(){
    return Math.sqrt(this.real * this.real + this.imag * this.imag);
  },
  angle: function(){
    return Math.atan2(this.imag, this.real);
  },
  isReal: function(){
    return this.imag == 0;
  },
  isRational: function() {
    return this.imag == 0 && BiwaScheme.isRational(this.real);
  },
  isInteger: function(){
    return this.imag == 0 && BiwaScheme.isInteger(this.real);
  },
  toString: function(radix){
    if (this.real === 0 && this.imag === 0)
      return "0";
    var img = "";
    if (this.imag !== 0) {
      if (this.imag > 0 && this.real !== 0){
          img+="+";
      }
      switch(this.imag) {
          case 1:
              break;
          case -1: img+="-";
               break;
          default: img+=this.imag.toString(radix);
      }
     img+="i";
    }
    var real = "";
    if (this.real !== 0){
      real += this.real.toString(radix);
    }
    return real+img;
  }
})
BiwaScheme.Complex.from_polar = function(r, theta){
  var real = r * Math.cos(theta);
  var imag = r * Math.sin(theta);
  return new BiwaScheme.Complex(real, imag);
}
BiwaScheme.Complex.assure = function(num){
  if(num instanceof BiwaScheme.Complex)
    return num
  else
    return new BiwaScheme.Complex(num, 0);
}

//
// Rational (unfinished)
//
BiwaScheme.Rational = BiwaScheme.Class.create({
  initialize: function(numerator, denominator){
    this.numerator = numerator;
    this.denominator = denominator;
  },

  isInteger: function() {
     // FIXME
  }
})

//
// Predicates
//
BiwaScheme.isNumber = function(x) {
  return (x instanceof BiwaScheme.Complex)  ||
         (x instanceof BiwaScheme.Rational) ||
         (typeof(x) == 'number');
};
BiwaScheme.isComplex = BiwaScheme.isNumber;
BiwaScheme.isReal = function(x) {
  if (x instanceof BiwaScheme.Complex || x instanceof BiwaScheme.Rational) {
    return x.isReal()
  }
  else {
    return (typeof(x) == 'number');
  }
};
BiwaScheme.isRational = function(x) {
  if (x instanceof BiwaScheme.Complex) {
    return x.isRational();
  }
  else if (x instanceof BiwaScheme.Rational) {
    return true;
  }
  else {
    return (typeof(x) == 'number');
  }
};
BiwaScheme.isInteger = function(x) {
  if (x instanceof BiwaScheme.Complex || x instanceof BiwaScheme.Rational) {
    return x.isInteger();
  }
  else {
    return (typeof(x) == 'number') && (x % 1 == 0);
  }
};
//
// Port
//

// (eof-object)
BiwaScheme.eof = new Object;

BiwaScheme.Port = BiwaScheme.Class.create({
  initialize: function(is_in, is_out){
    this.is_open = true;
    this.is_binary = false; //??
    this.is_input = is_in;
    this.is_output = is_out;
  },
  close: function(){
    // close port
    this.is_open = false;
  },
  inspect: function(){
    return "#<Port>";
  },
  to_write: function(){
    return "#<Port>";
  }
});

//
// string ports (srfi-6)
//
BiwaScheme.Port.StringOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(){
    this.buffer = [];
  },
  put_string: function(str){
    this.buffer.push(str);
  },
  output_string: function(str){
    return this.buffer.join("");
  }
});

BiwaScheme.Port.StringInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(str){
    this.str = str;
  },
  get_string: function(after){
    return after(this.str);
  }
});

BiwaScheme.Port.NullInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(){
  },
  get_string: function(after){
    // Never give them anything!
    return after('');
  }
});

BiwaScheme.Port.NullOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){}
});

BiwaScheme.Port.CustomOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){
    this.output_function(str);
  }
});

BiwaScheme.Port.CustomInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(input_function){
    this.input_function = input_function;
  },
  get_string: function(after){
    var input_function = this.input_function;
    return new BiwaScheme.Pause(function(pause) {
      input_function(function(input) {
        pause.resume(after(input));
      });
    });
  }
});

// User must set the current input/output
BiwaScheme.Port.current_input  = new BiwaScheme.Port.NullInput();
BiwaScheme.Port.current_output = new BiwaScheme.Port.NullOutput();
BiwaScheme.Port.current_error  = new BiwaScheme.Port.NullOutput();
//
// R6RS Records
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_chap_6
//
// Record is like struct in C, but supports more feature like inheritance.
// see also: src/library/r6rs_lib.js

//
// Record 
// represents each instance of record type
//
BiwaScheme.Record = BiwaScheme.Class.create({
  initialize: function(rtd, values){
    BiwaScheme.assert_record_td(rtd, "new Record");

    this.rtd = rtd;
    this.fields = values;
  },

  get: function(k){
    return this.fields[k]
  },

  set: function(k, v){
    this.fields[k] = v;
  },

  toString: function(){
    var contents = BiwaScheme.to_write(this.fields);
    return "#<Record "+this.rtd.name+" "+contents+">";
  }
});

BiwaScheme.isRecord = function(o){
  return (o instanceof BiwaScheme.Record);
};

// Defined record types
BiwaScheme.Record._DefinedTypes = {};

BiwaScheme.Record.define_type = function(name_str, rtd, cd){
  return BiwaScheme.Record._DefinedTypes[name_str] = {rtd: rtd, cd: cd};
};
BiwaScheme.Record.get_type = function(name_str){
  return BiwaScheme.Record._DefinedTypes[name_str];
};

//
// RTD (Record type descriptor)
//
BiwaScheme.Record.RTD = BiwaScheme.Class.create({
  //                   Symbol RTD        Symbol Bool  Bool    Array
  initialize: function(name, parent_rtd, uid, sealed, opaque, fields){
    this.name = name;
    this.parent_rtd = parent_rtd;
    this.is_base_type = !parent_rtd;

    if(uid){
      this.uid = uid;
      this.generative = false;
    }
    else{
      this.uid = this._generate_new_uid();;
      this.generative = true;
    }

    this.sealed = !!sealed;
    this.opaque = parent_rtd.opaque || (!!opaque);

    this.fields = _.map(fields, function(field){
      return {name: field[0], mutable: !!field[1]};
    });
  },

  // Returns the name of the k-th field.
  // Only used for error messages.
  field_name: function(k){
    var names = this._field_names();

    for(par = this.parent_rtd; par; par = par.parent_rtd){
      names = par._field_names() + names;
    }

    return names[k];
  },
  _field_names: function(){
    return _.map(this.fields, function(spec){
        return spec.name;
      });
  },

  _generate_new_uid: function(){
    return BiwaScheme.Sym(_.uniqueId("__record_td_uid"));
  },

  toString: function(){
    return "#<RecordTD "+name+">";
  }
});

BiwaScheme.Record.RTD.NongenerativeRecords = {};
BiwaScheme.isRecordTD = function(o){
  return (o instanceof BiwaScheme.Record.RTD);
};

//
// CD (Record constructor descriptor)
//
BiwaScheme.Record.CD = BiwaScheme.Class.create({
  initialize: function(rtd, parent_cd, protocol){
    this._check(rtd, parent_cd, protocol);
    this.rtd = rtd;
    this.parent_cd = parent_cd;
    if(protocol){
      this.has_custom_protocol = true;
      this.protocol = protocol;
    }
    else{
      this.has_custom_protocol = false;
      if(rtd.parent_rtd)
        this.protocol = this._default_protocol_for_derived_types();
      else
        this.protocol = this._default_protocol_for_base_types();
    }
  },

  _check: function(rtd, parent_cd, protocol){
    if(rtd.is_base_type && parent_cd)
      throw new Error("Record.CD.new: cannot specify parent cd of a base type");

    if(parent_cd && rtd.parent_rtd && (parent_cd.rtd != rtd.parent_rtd))
      throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd");

    if(rtd.parent_rtd && !parent_cd && protocol)
      throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given");

    if(parent_cd && parent_cd.has_custom_protocol && !protocol)
      throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol");
  },
  
  _default_protocol_for_base_types: function(){
    // (lambda (p) p)
    // called with `p' as an argument
    return function(ar){
      var p = ar[0];
      BiwaScheme.assert_procedure(p, "_default_protocol/base");
      return p;
    };
  },

  _default_protocol_for_derived_types: function(){
    // (lambda (n) 
    //   (lambda (a b x y s t)
    //     (let1 p (n a b x y) (p s t))))
    // called with `n' as an argument
    var rtd = this.rtd;
    return function(ar){
      var n = ar[0];
      BiwaScheme.assert_procedure(n, "_default_protocol/n");

      var ctor = function(args){
        var my_argc = rtd.fields.length;
        var ancestor_argc = args.length - my_argc;

        var ancestor_values = args.slice(0, ancestor_argc);
        var my_values       = args.slice(ancestor_argc);

        // (n a b x y) => p
        return new BiwaScheme.Call(n, ancestor_values, function(ar){
          var p = ar[0];
          BiwaScheme.assert_procedure(p, "_default_protocol/p");

          // (p s t) => record
          return new BiwaScheme.Call(p, my_values, function(ar){
            var record = ar[0];
            BiwaScheme.assert_record(record, "_default_protocol/result");

            return record;
          });
        });
      };
      return ctor;
    };
  },

  toString: function(){
    return "#<RecordCD "+this.rtd.name+">";
  },

  record_constructor: function(){
    var arg_for_protocol = (this.parent_cd ? this._make_n([], this.rtd)
                                           : this._make_p());
    arg_for_protocol = _.bind(arg_for_protocol, this);

    return new BiwaScheme.Call(this.protocol, [arg_for_protocol], function(ar){
      var ctor = ar[0];
      BiwaScheme.assert_procedure(ctor, "record_constructor");
      return ctor;
    });
  },

  // Create the function `p' which is given to the protocol.
  _make_p: function(){
    return function(values){
      return new BiwaScheme.Record(this.rtd, values);
      // TODO: check argc 
    };
  },

  // Create the function `n' which is given to the protocol.
  // When creating an instance of a derived type,
  // _make_n is called for each ancestor rtd's.
  _make_n: function(children_values, rtd){
    var parent_cd = this.parent_cd;

    if(parent_cd){
      // called from protocol (n)
      var n = function(args_for_n){

        // called from protocol (p)
        var p = function(args_for_p){
          var values = [].concat(args_for_p[0]).concat(children_values)
          var parent_n = parent_cd._make_n(values, rtd);

          return new BiwaScheme.Call(parent_cd.protocol, [parent_n], function(ar){
            var ctor = ar[0];
            BiwaScheme.assert_procedure(ctor, "_make_n");

            return new BiwaScheme.Call(ctor, args_for_n, function(ar){
              var record = ar[0];
              BiwaScheme.assert_record(record);
              return record;
            });
          });
        };
        return p;
      };
      return n;
    }
    else{
      var n = function(my_values){
        var values = my_values.concat(children_values);
        return new BiwaScheme.Record(rtd, values);
        // TODO: check argc 
      };
      return n;
    }
  }
});

BiwaScheme.isRecordCD = function(o){
  return (o instanceof BiwaScheme.Record.CD);
};
// 
// R6RS Enumerations
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-15.html#node_chap_14
//
// Example
//
//   (define-enumeration color
//     (black white purple maroon)
//     color-set)
//   
//   (color black)                  ;=> 'black
//   (color purpel)                 ;=> &syntax exception
//   (enum-set->list
//     (color-set maroon white))    ;=> #<enum-set (white maroon)>

BiwaScheme.Enumeration = {};

// Represents an enum_type.
//
// Becuase there is no way to access an EnumType directly from Scheme,
// EnumType#to_write is not defined.
//
// Properties
//
// members - Array of symbols (no duplicate)
//
BiwaScheme.Enumeration.EnumType = BiwaScheme.Class.create({
  // Creates a new enum_type.
  //
  // members - Array of symbols.
  //           Symbols may be duplicate (I think you shouldn't, though :-p).
  initialize: function(members){
    this.members = _.uniq(members);
  },

  // Returns an EnumSet.
  universe: function(){
    return new BiwaScheme.Enumeration.EnumSet(this, this.members);
  }, 

  // Returns a function which map a symbol to an integer (or #f, if 
  // the symbol is out of the universe).
  // 
  // Implementation note: don't forget this.members may have duplicates.
  indexer: function(){
    // ar[0] - a symbol
    // Returns an integer or #f.
    return _.bind(function(ar){
      BiwaScheme.assert_symbol(ar[0], "(enum-set indexer)");
      var idx = _.indexOf(this.members, ar[0]);
      return (idx === -1) ? false : idx;
    }, this);
  },

  // Retuns a function which creates an enum_set from a list of
  // symbols (Symbols may be duplicate.)
  constructor: function(){
    // ar[0] - a list of symbol
    // Returns a enum_set.
    return _.bind(function(ar){
      BiwaScheme.assert_list(ar[0], "(enum-set constructor)");
      var symbols = ar[0].to_array();
      _.each(symbols, function(arg){
        BiwaScheme.assert_symbol(arg, "(enum-set constructor)");
      });

      return new BiwaScheme.Enumeration.EnumSet(this, symbols);
    }, this);
  }
});
BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumType,
  ["universe", "indexer", "constructor"]); 

// Represents an enum_set of an enum_type.
//
// Properties
//
// enum_type - The enum_type.
// symbols   - Array of symbols (no duplicate, properly ordered)
//
BiwaScheme.Enumeration.EnumSet = BiwaScheme.Class.create({
  // Creates a new enum_set.
  //
  // enum_type - An EnumType
  // symbols   - Array of symbols.
  //
  // initialize normalizes symbols.
  //   - remove duplicates
  //   - order by universe
  initialize: function(enum_type, symbols){
    this.enum_type = enum_type;
    this.symbols = _.filter(enum_type.members, function(sym){
      return _.include(symbols, sym);
    });
  },

  // Returns a list of symbols.
  symbol_list: function(){
    return BiwaScheme.array_to_list(this.symbols); 
  },
  
  // Returns true if the enum_set includes the symbol.
  // 'symbol' is allowed to be a symbol which is not included in the universe.
  is_member: function(symbol){
    return _.include(this.symbols, symbol);
  },
  
  // Returns true if:
  // - the enum_set is a subset of the enum_set 'other', and
  // - the universe of the enum_set is a subset of 
  //   the universe of 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  is_subset: function(other){
    // Check elements
    if(_.any(this.symbols, function(sym){
         return !_.include(other.symbols, sym);
       })){
      return false;
    }

    // Check universe
    if(this.enum_type === other.enum_type){
      return true;
    }
    else{
      return _.all(this.enum_type.members, function(sym){
               return _.include(other.enum_type.members, sym);
             });
    }
  },

  // Returns true if:
  //   - the enum_set contains the same set of symbols as 'other', and
  //   - universe of the enum_set contains the same set of symbols
  //     as the universe of 'other'.
  //
  // The enum_set and 'other' may belong to different enum_type.
  equal_to: function(other){
    return this.is_subset(other) && other.is_subset(this);
  },

  // Returns a enum_set which has:
  // - all the symbols included in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  union: function(other){
    var syms = _.filter(this.enum_type.members, _.bind(function(sym){
                 return _.include(this.symbols, sym) ||
                        _.include(other.symbols, sym);
               }, this));
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included both in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  intersection: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return _.include(other.symbols, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and not in the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  difference: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return !_.include(other.symbols, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the universe but not in the enum_set.
  complement: function(){
    var syms = _.filter(this.enum_type.members, _.bind(function(sym){
                 return !_.include(this.symbols, sym);
               }, this));
    return new BiwaScheme.Enumeration.EnumSet(this.enum_type, syms);
  },

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and the universe of the enum_set 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  projection: function(other){
    var syms = _.filter(this.symbols, function(sym){
                 return _.include(other.enum_type.members, sym);
               });
    return new BiwaScheme.Enumeration.EnumSet(other.enum_type, syms);
  },

  // Returns a string which represents the enum_set.
  toString: function(){
    return "#<EnumSet "+BiwaScheme.inspect(this.symbols)+">";
  }
});
BiwaScheme.Class.memoize(BiwaScheme.Enumeration.EnumSet, "symbol_list");

BiwaScheme.isEnumSet = function(obj){
  return (obj instanceof BiwaScheme.Enumeration.EnumSet);
};
//
// Hashtable
//
// Based on the base JavaScript Object class, but
//  * Object takes only strings as keys
//  * R6RS hashtable needs its own hash function
// so some hacks are needed.

BiwaScheme.Hashtable = BiwaScheme.Class.create({
  initialize: function(_hash_proc, _equiv_proc, mutable){
    this.mutable = (mutable === undefined) ? true :
                   mutable ? true : false;

    this.hash_proc = _hash_proc;
    this.equiv_proc = _equiv_proc;

    // Hash (hashed) => (array of (key and value))
    this.pairs_of = {};
  },

  clear: function(){
    this.pairs_of = {};
  },

  candidate_pairs: function(hashed){
    return this.pairs_of[hashed];
  },

  add_pair: function(hashed, key, value){
    var pairs = this.pairs_of[hashed];

    if (pairs) {
      pairs.push([key, value]);
    }
    else {
      this.pairs_of[hashed] = [[key, value]];
    }
  },

  remove_pair: function(hashed, pair){
    var pairs = this.pairs_of[hashed];
    var i = pairs.indexOf(pair);
    if (i == -1){
      throw new BiwaScheme.Bug("Hashtable#remove_pair: pair not found!");
    }
    else {
      pairs.splice(i, 1); //remove 1 element from i-th index
    }
  },

  create_copy: function(mutable){
    var copy = new BiwaScheme.Hashtable(this.hash_proc, this.equiv_proc,
                                        mutable);
    // clone the pairs to copy
    _.each(_.keys(this.pairs_of), _.bind(function(hashed){
      var pairs = this.pairs_of[hashed];
      var cloned = _.map(pairs, function(pair){
        return _.clone(pair);
      });
      copy.pairs_of[hashed] = cloned;
    }, this));

    return copy;
  },

  size: function(){
    var n = 0;
    this._apply_pair(function(pair){
      n++;
    });
    return n;
  },

  keys: function(){
    return this._apply_pair(function(pair){
      return pair[0];
    });
  },

  values: function(){
    return this._apply_pair(function(pair){
      return pair[1];
    });
  },

  _apply_pair: function(func){
    var a = [];
    _.each(_.values(this.pairs_of), function(pairs){
      _.each(pairs, function(pair){
        a.push(func(pair));
      });
    });
    return a;
  },

  to_write: function(){
    return "#<Hashtable size=" + this.size() + ">";
  }
});

//
// Hash functions
//

BiwaScheme.Hashtable.equal_hash = function(ar){
  return BiwaScheme.to_write(ar[0]);
};
BiwaScheme.Hashtable.eq_hash = BiwaScheme.Hashtable.equal_hash;
BiwaScheme.Hashtable.eqv_hash = BiwaScheme.Hashtable.equal_hash;

BiwaScheme.Hashtable.string_hash = function(ar){
  return ar[0];
};

BiwaScheme.Hashtable.string_ci_hash = function(ar){
  return _.isString(ar[0]) ? ar[0].toLowerCase() : ar[0];
};

BiwaScheme.Hashtable.symbol_hash = function(ar){
  return (ar[0] instanceof BiwaScheme.Symbol) ? ar[0].name : ar[0];
};

//
// Equivalence functions
//

BiwaScheme.Hashtable.eq_equiv = function(ar){
  return BiwaScheme.eq(ar[0], ar[1]);
};

BiwaScheme.Hashtable.eqv_equiv = function(ar){
  return BiwaScheme.eqv(ar[0], ar[1]);
};
//
// Syntax
//
BiwaScheme.Syntax = BiwaScheme.Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },
  transform: function(x){
    if (!this.func){
      throw new BiwaScheme.Bug("sorry, syntax "+this.sname+
                               " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
})

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
BiwaScheme.CoreEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.CoreEnv["begin"]  = new BiwaScheme.Syntax("begin");
BiwaScheme.CoreEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.CoreEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.CoreEnv["if"]     = new BiwaScheme.Syntax("if");
BiwaScheme.CoreEnv["set!"]   = new BiwaScheme.Syntax("set!");
  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = BiwaScheme.Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function(){
      return [
        "#<Parser:",
        this.i, "/", this.tokens.length, " ",
        BiwaScheme.inspect(this.tokens),
        ">"
      ].join("");
    },

    tokenize: function(txt) {
      var tokens = new Array(), oldTxt=null;
      var in_srfi_30_comment = 0;

      while( txt != "" && oldTxt != txt ) {
        oldTxt = txt;
        txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
        function($0,$1) {
          var t = $1;

          if (t == "#|") {
            in_srfi_30_comment++;
            return "";
          }
          else if (in_srfi_30_comment > 0) {
            if ( /(.*\|#)/.test(t) ) {
              in_srfi_30_comment--;
              if (in_srfi_30_comment < 0) {
                throw new BiwaScheme.Error("Found an extra comment terminator: `|#'")
              }
              // Push back the rest substring to input stream.
              return t.substring(RegExp.$1.length, t.length);
            }
            else {
              return "";
            }
          }
          else {
            if( t.charAt(0) != ';' ) tokens[tokens.length]=t;
            return "";
          }
        } );
      }
      return tokens;
    },

    sexpCommentMarker: new Object,
    getObject: function() {
      var r = this.getObject0();

      if (r != this.sexpCommentMarker)
        return r;

      r = this.getObject();
      if (r == BiwaScheme.Parser.EOS)
        throw new BiwaScheme.Error("Readable object not found after S exression comment");

      r = this.getObject();
      return r;
    },
    
    getList: function( close ) {
      var list = BiwaScheme.nil, prev = list;
      while( this.i < this.tokens.length ) {

        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");

        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != BiwaScheme.Parser.EOS && list != BiwaScheme.nil ) {
            prev.cdr = o;
          }
        } else {
            var cur = new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil);
            if( list == BiwaScheme.nil ) list = cur;
            else prev.cdr = cur;
            prev = cur;
        }
      }
      return list;
    },

    getVector: function( close ) {
      var arr = new Array();
      while( this.i < this.tokens.length ) {
        
        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");
        
        if( this.tokens[ this.i ] == ')' ||
        this.tokens[ this.i ] == ']' ||
        this.tokens[ this.i ] == '}' ) { this.i++; break; }
        arr[ arr.length ] = this.getObject();
      }
      return arr;
    },

    eatObjectsInSexpComment: function(err_msg) {
      while( this.tokens[ this.i ] == '#;' ) {
        this.i++;
        if ((this.getObject() == BiwaScheme.Parser.EOS) || (this.i >= this.tokens.length))
          throw new BiwaScheme.Error(err_msg);  
      }
    }, 

    getObject0: function() {
      if( this.i >= this.tokens.length )
        return BiwaScheme.Parser.EOS;

      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

      if (t == '#;')
        return this.sexpCommentMarker;

      var s = t == "'"  ? 'quote' :
              t == "`"  ? 'quasiquote' :
              t == ","  ? 'unquote' :
              t == ",@" ? 'unquote-splicing' : false;

      if( s || t == '(' || t == '#(' || t == '[' || t == '#[' || t == '{' || t == '#{' ) {
        return s ? new BiwaScheme.Pair( BiwaScheme.Sym(s), new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil ))
        : (t=='(' || t=='[' || t=='{') ? this.getList(t) : this.getVector(t);
      } 
      else {
        switch(t){
          case "+inf.0" : return Infinity;
          case "-inf.0" : return -Infinity;
          case "+nan.0" : return NaN;
        }

        var n;
        if( /^#x[0-9a-z]+$/i.test(t) ) {  // #x... Hex
          n = new Number('0x'+t.substring(2,t.length) );
        } 
        else if( /^#d[0-9\.]+$/i.test(t) ) {  // #d... Decimal
          n = new Number( t.substring(2,t.length) );
        } 
        else{
          n = new Number(t);  // use constrictor as parser
        }

        if( ! isNaN(n) ) {
          return n.valueOf();
        } else if( t == '#f' || t == '#F' ) {
          return false;
        } else if( t == '#t' || t == '#T' ) {
          return true;
        } else if( t.toLowerCase() == '#\\newline' ) {
          return BiwaScheme.Char.get('\n');
        } else if( t.toLowerCase() == '#\\space' ) {
          return BiwaScheme.Char.get(' ');
        } else if( t.toLowerCase() == '#\\tab' ) {
          return BiwaScheme.Char.get('\t');
        } else if( /^#\\.$/.test(t) ) {
          return BiwaScheme.Char.get( t.charAt(2) );
        } else if( /^#\\x[a-zA-Z0-9]+$/.test(t) ) {
          var scalar = parseInt(t.slice(3), 16);
          // R6RS 11.11 (surrogate codepoints)
          if (scalar >= 0xD800 && scalar <= 0xDFFF) {
            throw new BiwaScheme.Error("Character in Unicode excluded range.");
          }
          // ECMA-262 4.3.16 -- Basically, strings are sequences of 16-bit
          // unsigned integers, so anything greater than 0xFFFF won't fit.
          // NOTE: This violates R6RS which requires the full Unicode range!
          else if (scalar > 0xFFFF) {
            throw new BiwaScheme.Error("Character literal out of range.");
          } else {
            return BiwaScheme.Char.get(String.fromCharCode(scalar));
          }
        } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
          return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
            return $1 ? $1 : '';
          } );
        } else return BiwaScheme.Sym(t);  // 2Do: validate !!
      }
    }
  });
  // indicates end of source file
  BiwaScheme.Parser.EOS = new Object();
  

///
/// Compiler
///
/// Note: macro expansion is done by Intepreter#expand

BiwaScheme.Compiler = BiwaScheme.Class.create({
  initialize: function(){
  },

  is_tail: function(x){
    return (x[0] == "return");
  },

  //free: set
  //e: env(= [locals, frees])
  //next: opc
  //ret: opc["refer_*", n, ["argument", 
  //          ["refer_*", n, ... ["argument", next]
  collect_free: function(free, e, next){
    var vars = free;
    var opc = next;
    var arr = vars.arr;
    for(var i=0; i<arr.length; i++){
      opc = this.compile_refer(arr[i], e, ["argument", opc]);
    }
    //Console.puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
    return opc;
  },

  //x: Symbol
  //e: env [set of locals, set of frees]
  //ret: opc
  compile_refer: function(x, e, next){
    return this.compile_lookup(x, e,
             function(n){ return ["refer-local", n, next] },
             function(n){ return ["refer-free",  n, next] },
             function(sym){ return ["refer-global", sym, next] });
  },

  compile_lookup: function(x, e, return_local, return_free, return_global){
    var locals = e[0], free = e[1];
    if((n = locals.index(x)) != null){
      //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
      return return_local(n);
    }
    else if((n = free.index(x)) != null){
      //Console.puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
      return return_free(n);
    }
    else{
      var sym = x.name;
      return return_global(sym);
    }
    //throw new BiwaScheme.Error("undefined symbol `" + sym + "'");
  },

  //generate boxing code (intersection of sets & vars)
  //if no need of boxing, just returns next
  //  sets(Set): assigned variables 
  //  vars(List): used variables
  //  next(opc):
  //  ret(opc):
  make_boxes: function(sets, vars, next){
    var vars = vars;
    var n = 0;
    var a = [];
    while(vars instanceof BiwaScheme.Pair){
      if(sets.member(vars.car))
        a.push(n);
      n++;
      vars = vars.cdr;
    }
    var opc = next;
    for(var i=a.length-1; i>=0; i--)
      opc = ["box", a[i], opc];
    return opc;
  },

  // Enumerate variables which (could be assigned && included in v)
  // x: exp
  // v: set(vars)
  // ret: set
  find_sets: function(x, v){
    //Console.puts("find_sets: " + to_write(x) + " " + to_write(v))
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_sets(exp, v);
      case BiwaScheme.Sym("begin"):
        ret = this.find_sets(x.cdr, v); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_sets(body, v.set_minus(vars.to_set()));
        }
        else { // (lambda args ...)
          ret = this.find_sets(body, v.set_minus(new BiwaScheme.Set(vars)));
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_sets(testc, v).set_union(
                        this.find_sets(thenc, v),
                        this.find_sets(elsec, v));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), xx=x.third();
        if(v.member(vari))
          ret = this.find_sets(xx, v).set_cons(vari);
        else
          ret = this.find_sets(xx, v);
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_sets(exp, v);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_sets(p.car, v));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }

    if(ret == null)
      throw new BiwaScheme.Bug("find_sets() exited in unusual way");
    else
      return ret;
  },

  // find_free(): find free variables in x
  //              these variables are collected by collect_free().
  // x: expression 
  // b: set of local vars (= variables which are not free)
  // f: set of free var candidates 
  //    (local vars of outer lambdas)
  // ret: set of free vars
  find_free: function(x, b, f){
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      if(f.member(x))
        ret = new BiwaScheme.Set(x);
      else
        ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_free(exp, b, f);
        break;
      case BiwaScheme.Sym("begin"):
        ret = this.find_free(x.cdr, b, f); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_free(body, b.set_union(vars.to_set()), f);
        }
        else { // (lambda args ...)
          ret = this.find_free(body, b.set_cons(vars), f);
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_free(testc, b, f).set_union(
                        this.find_free(thenc, b, f),
                        this.find_free(elsec, b, f));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), exp=x.third();
        if(f.member(vari))
          ret = this.find_free(exp, b, f).set_cons(vari);
        else
          ret = this.find_free(exp, b, f)
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_free(exp, b, f);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_free(p.car, b, f));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }
    //Console.p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

    if(ret == null)
      throw new BiwaScheme.Bug("find_free() exited in unusual way");
    else
      return ret;
  },

  // Returns the position of the dot pair.
  // Returns -1 if x is a proper list.
  //
  // eg. (a b . c) -> 2
  find_dot_pos: function(x){
    var idx = 0;
    for (; x instanceof BiwaScheme.Pair; x = x.cdr, ++idx)
      ;
    if (x != BiwaScheme.nil) {
      return idx;
    } else {
      return -1;
    }
  },

  last_pair: function(x){
    if (x instanceof BiwaScheme.Pair){
      for (; x.cdr instanceof BiwaScheme.Pair; x = x.cdr)
        ;
    }
    return x;
  },

  // Takes an dotted list and returns proper list.
  //
  // eg. (x y . z) -> (x y z)
  dotted2proper: function(ls){
    var nreverse = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ){
        var d = ls.cdr;
        ls.cdr = res;
        res = ls;
        ls = d;
      }
      return res;
    }
    var copy_list = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ls = ls.cdr){
        res = new BiwaScheme.Pair(ls.car, res);
      }
      return nreverse(res);
    }

    if (ls instanceof BiwaScheme.Pair) {
      var last = this.last_pair(ls);
      if (last instanceof BiwaScheme.Pair && last.cdr === BiwaScheme.nil){
        return ls;
      } else {
        var copied = copy_list(ls);
        this.last_pair(copied).cdr = new BiwaScheme.Pair(last.cdr, BiwaScheme.nil);
        return copied;
      }
    } else {
      return new BiwaScheme.Pair(ls, BiwaScheme.nil);
    }
  },

  // x: exp(list of symbol or integer or..)
  // e: env (= [locals, frees])
  // s: vars might be set!
  // next: opc
  // ret: opc
  compile: function(x, e, s, f, next){
    //Console.p(x);
    var ret = null;

    while(1){
      if(x instanceof BiwaScheme.Symbol){
        // Variable reference
        // compiled into refer-(local|free|global)
        return this.compile_refer(x, e, (s.member(x) ? ["indirect", next] : next));
      }
      else if(x instanceof BiwaScheme.Pair){
        switch(x.first()){
        case BiwaScheme.Sym("define"):
          ret = this._compile_define(x, next);

          x = ret[0];
          next = ret[1];
          break;

        case BiwaScheme.Sym("begin"):
          var a = [];
          for(var p=x.cdr; p instanceof BiwaScheme.Pair; p=p.cdr)
            a.push(p.car);

          //compile each expression (in reverse order)
          var c = next;
          for(var i=a.length-1; i>=0; i--){
            c = this.compile(a[i], e, s, f, c);
          }
          return c;

        case BiwaScheme.Sym("quote"):
          if(x.length() < 2)
              throw new BiwaScheme.Error("Invalid quote: "+x.to_write());

          var obj=x.second();
          return ["constant", obj, next];

        case BiwaScheme.Sym("lambda"):
          return this._compile_lambda(x, e, s, f, next);

        case BiwaScheme.Sym("if"):
          if(x.length() < 3 || x.length() > 4)
              throw new BiwaScheme.Error("Invalid if: "+x.to_write());

          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          var thenc = this.compile(thenc, e, s, f, next);
          var elsec = this.compile(elsec, e, s, f, next);
          x    = testc;
          next = ["test", thenc, elsec];
          break;

        case BiwaScheme.Sym("set!"):
          // error-checking: should have only 3 things
          if(x.length() != 3)
              throw new BiwaScheme.Error("Invalid set!: "+x.to_write());

          var v=x.second(), x=x.third();
          var do_assign = this.compile_lookup(v, e,
            function(n){ return ["assign-local", n, next]; },
            function(n){ return ["assign-free",  n, next]; },
            function(sym){ return ["assign-global",sym, next]; }
          );
          next = do_assign;
          break;

        case BiwaScheme.Sym("call/cc"): 
          var x=x.second();
          var c = ["conti", 
                    (this.is_tail(next) ? (e[0].size() + 1) : 0), //number of args for outer lambda
                    ["argument",
                    ["constant", 1,
                    ["argument",
                      this.compile(x, e, s,f,  
                        (this.is_tail(next) ? ["shift", 1, ["tco_hinted_apply"]]
                                            : ["apply"]))]]]];
                  //note: proc for call/cc takes 1 argument (= ["apply", 1])

          // Do not push stack frame when call/cc is in a tail context
          return this.is_tail(next) ? c : ["frame", c, next];

        default: 
          //apply 
          //x = (func 1 2) 
          //x.car = func = '(lambda (x) ..) or Symbol
          //x.cdr = args = '(1 2)
          var func = x.car;
          var args = x.cdr;
          var c = this.compile(func, e, s,f,  
                    this.is_tail(next) ? ["shift", args.length(), ["tco_hinted_apply"]]
                                       : ["apply"]);

          // VM will push the number of arguments to the stack.
          c = this.compile(args.length(), e, s, f, ["argument", c]);
          for(var p=args; p instanceof BiwaScheme.Pair; p=p.cdr){
            c = this.compile(p.car, e, s, f, ["argument", c]);
          }

          // Do not push stack frame for tail calls
          return this.is_tail(next) ? c : ["frame", c, next];
        }
      }
      else{
        return ["constant", x, next];
      }
    }
    //Console.p("result of " + x.inspect() + ":");
    //Console.p(ret);
    //dump({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]});
//      if(ret == null)
//        throw new BiwaScheme.Bug("compile() exited in unusual way");
//      else
//        return ret;
  },

  // Compile define.
  //
  // 0. (define) ; => error
  // 1. (define a)
  // 2. (define a 1)
  // 3. (define a 1 2) ; => error
  // 4. (define (f x) x), (define (f . a) a)
  // 5. (define 1 2) 
  //
  // Note: define may appear in lambda, let, let*, let-values,
  // let*-values, letrec, letrec*. These definitions are local to the
  // <body> of these forms.
  _compile_define: function(x, next){
    if(x.length() == 1) { // 0. (define)
      throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
    }

    var first = x.cdr.car;
    var rest = x.cdr.cdr;
    
    if(first instanceof BiwaScheme.Symbol){    
      if (rest === BiwaScheme.nil) { // 1. (define a)
        x = BiwaScheme.undef;
      }
      else {
        if (rest.cdr === BiwaScheme.nil) // 2. (define a 1)
          x = rest.car;
        else // 3. (define a 1 2)
          throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
      }

      if (!BiwaScheme.TopEnv.hasOwnProperty(first.name)) {
        BiwaScheme.TopEnv[first.name] = BiwaScheme.undef;
      }
      next = ["assign-global", first.name, next];
    }
    else if(first instanceof BiwaScheme.Pair){ // 4. (define (f x) ...)
      // Note: define of this form may contain internal define.
      // They are handled in compilation of "lambda".

      var fname=first.car, args=first.cdr;
      var lambda = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"), new BiwaScheme.Pair(args, rest));
      x = lambda;
      if (!BiwaScheme.TopEnv.hasOwnProperty(first.name)) {
        BiwaScheme.TopEnv[fname.name] = BiwaScheme.undef;
      }
      next = ["assign-global", fname.name, next];
    }
    else{ // 5. (define 1 2)
      throw new BiwaScheme.Error("define: symbol or pair expected but got "+first);
    }

    return [x, next];
  },

  // Compiles various forms of "lambda".
  //
  // * (lambda (x y) ...)
  // * (lambda (x y . rest) ...)
  // * (lambda args ...)
  _compile_lambda: function(x, e, s, f, next){
    if(x.length() < 3)
      throw new BiwaScheme.Error("Invalid lambda: "+x.to_write());

    var vars = x.cdr.car;
    var body = x.cdr.cdr;

    // Handle internal defines
    var tbody = BiwaScheme.Compiler.transform_internal_define(body);
    if(BiwaScheme.isPair(tbody) &&
       BiwaScheme.isSymbol(tbody.car) &&
       tbody.car.name == "letrec*"){
      // The body has internal defines.
      // Expand letrec* macro
      var cbody = BiwaScheme.Interpreter.expand(tbody);
    }
    else{
      // The body has no internal defines.
      // Just wrap the list with begin 
      var cbody = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), x.cdr.cdr);
    }

    var dotpos = this.find_dot_pos(vars);
    var proper = this.dotted2proper(vars);
    var free = this.find_free(cbody, proper.to_set(), f); //free variables
    var sets = this.find_sets(cbody, proper.to_set());    //local variables

    var do_body = this.compile(cbody,
                    [proper.to_set(), free],
                    sets.set_union(s.set_intersect(free)),
                    f.set_union(proper.to_set()),
                    ["return"]);
    var do_close = ["close", 
                     free.size(),
                     this.make_boxes(sets, proper, do_body),
                     next,
                     dotpos];
    return this.collect_free(free, e, do_close);
  },

  run: function(expr){
    return this.compile(expr, [new BiwaScheme.Set(), new BiwaScheme.Set()], new BiwaScheme.Set(), new BiwaScheme.Set(), ["halt"]);
  }
});

// Compile an expression with new compiler
BiwaScheme.Compiler.compile = function(expr, next){
  expr = BiwaScheme.Interpreter.expand(expr);
  return (new BiwaScheme.Compiler).run(expr, next);
};

// Transform internal defines to letrec*.
//
// Example
//   (let ((a 1))
//     (define (b) a)
//     (b))
//
//   (let ((a 1))
//     (letrec* ((b (lambda () a)))
//       (b)))
//
// x - expression starts with (define ...)
// 
// Returns a letrec* expression, or
// just returns x, when x does not contain definitions.
(function(){
// Returns true if x is a definition
var is_definition = function(x){
  return BiwaScheme.isPair(x) &&
         BiwaScheme.Sym("define") === x.car;
  // TODO: support "begin", nested "begin", "let(rec)-syntax"
};

// Convert function definition to lambda binding
//   (define a ..)         -> (a ..)
//   (define (f) ..)       -> (f (lambda () ..))
//   (define (f x . y) ..) -> (f (lambda (x . y) ..))
//   (define (f . a) ..)   -> (f (lambda a ..))
var define_to_lambda_bind = function(def){
  var sig  = def.cdr.car;
  var body = def.cdr.cdr;

  if (BiwaScheme.isSymbol(sig)) {
    var variable = sig;

    return new BiwaScheme.Pair(variable, body);
  }
  else {
    var variable = sig.car;
    var value = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
                  new BiwaScheme.Pair(sig.cdr, body));

    return BiwaScheme.List(variable, value);
  }
};

BiwaScheme.Compiler.transform_internal_define = function(x){
  // 1. Split x into definitions and expressions
  var defs = [], item = x;
  while (is_definition(item.car)){
    defs.push(item.car);
    item = item.cdr;
  }
  var exprs = item;

  // 2. Return x if there is no definitions
  if (defs.length == 0)
    return x;
  
  // 3. Return (letrec* <bindings> <expressions>)
  var bindings = BiwaScheme.List.apply(null, _.map(defs, define_to_lambda_bind));
  return new BiwaScheme.Pair(BiwaScheme.Sym("letrec*"),
           new BiwaScheme.Pair(bindings, exprs));
};
})();
//
// pause object (facility to stop/resume interpreting)
//
BiwaScheme.Pause = BiwaScheme.Class.create({
  //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
  initialize: function(on_pause){
    this.on_pause = on_pause;
  },

  //save state of interpreter
  set_state: function(intp, x, f, c, s){
    this.interpreter = intp;
    this.x = x;
    this.f = f;
    this.c = c;
    this.s = s;
  },

  //call this when ready (to fire setTimeout, Ajax.Request..)
  ready: function(){
    this.on_pause(this);
  },

  //restart calculation
  resume: function(value){
    return this.interpreter.resume(true, value, this.x, this.f, this.c, this.s)
  }
});

///
/// Call
///

// The class Call is used to invoke scheme closure from 
// library functions.
//
// Call#initialize takes three arguments: proc, args and after.
//   * proc is the scheme closure to invoke.
//   * args is an Array (not list!) of arguments for the invocation.
//   * after is a javascript function which is invoked when 
//     returned from the proc.
//
//     after takes two arguments: ar and intp.
//       * ar is an Array which contains the result of the invocation.
//       * intp is an Interpreter which is running.
//
//     If after returns another Call object, another invocation
//     happens. If after returns a normal value, it is the value
//     of the library function.
//
// example:
//   return new Call(proc, [x, y], function(ar){ ar[0] });
//
BiwaScheme.Call = BiwaScheme.Class.create({
  initialize: function(proc, args, after){
    this.proc = proc;
    this.args = args;
    this.after = after || function(ar){
      // just return result which closure returned
      return ar[0];
    };
  },

  inspect: function(){
    return "#<Call args=" + this.args.inspect() + ">";
  },

  toString: function(){
    return "#<Call>";
  },

  to_write: function(){
    return "#<Call>";
  }
})

//
// Iterator - external iterator for Call.foreach
//
BiwaScheme.Iterator = {
  ForArray: BiwaScheme.Class.create({
    initialize: function(arr){
      this.arr = arr;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.arr.length;
    },
    next: function(){
      return this.arr[this.i++];
    }
  }),
  ForString: BiwaScheme.Class.create({
    initialize: function(str){
      this.str = str;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.str.length;
    },
    next: function(){
      return BiwaScheme.Char.get(this.str.charAt(this.i++));
    }
  }),
  ForList: BiwaScheme.Class.create({
    initialize: function(ls){
      this.ls = ls;
    },
    has_next: function(){
      return (this.ls instanceof BiwaScheme.Pair) &&
             this.ls != BiwaScheme.nil;
    },
    next: function(){
      var pair = this.ls;
      this.ls = this.ls.cdr;
      return pair;
    }
  }),
  ForMulti: BiwaScheme.Class.create({
    initialize: function(objs){
      this.objs = objs;
      this.size = objs.length;
      this.iterators = _.map(objs, function(x){
        return BiwaScheme.Iterator.of(x);
      })
    },
    has_next: function(){
      for(var i=0; i<this.size; i++)
        if(!this.iterators[i].has_next())
          return false;
      
      return true;
    },
    next: function(){
      return _.map(this.iterators, function(ite){
        return ite.next();
      })
    }
  }),
  of: function(obj){
    switch(true){
      case (obj instanceof Array):
        return new this.ForArray(obj);
      case (typeof(obj) == "string"):
        return new this.ForString(obj);
      case (obj instanceof BiwaScheme.Pair):
      case (obj === BiwaScheme.nil):
        return new this.ForList(obj);
      default:
        throw new BiwaScheme.Bug("Iterator.of: unknown class: "+BiwaScheme.inspect(obj));
    }
  }
}

//
// Call.foreach - shortcut for successive Calls
//
// Some library functions, such as for-each or map,
// call a closure for each element. Call.foreach is 
// a utility to help defining such methods.
//
// Call.foreach takes a sequence and some callbacks.
// Sequence is an Array, String, or list.
//
// Example:
//   return Call.foreach(sequence, {
//     // before each call
//     call: function(elem){
//       return new Call(proc, [elem]);
//     },
//     // after each call
//     result: function(value, elem){
//       ary.push(value);
//       // you can return a value to terminate the loop
//     },
//     // after all the calls
//     finish: function(){
//       return ary;
//     }
//   });

BiwaScheme.Call.default_callbacks = {
  call: function(x){ return new BiwaScheme.Call(this.proc, [x]) },
  result: function(){},
  finish: function(){}
}
BiwaScheme.Call.foreach = function(obj, callbacks, is_multi){
  is_multi || (is_multi = false);
  _.each(["call", "result", "finish"], function(key){
    if(!callbacks[key])
      callbacks[key] = BiwaScheme.Call.default_callbacks[key];
  })
  
  var iterator = null;
  var x = null;

  var loop = function(ar){
    if(iterator){
      var ret = callbacks["result"](ar[0], x);
      if(ret !== undefined) return ret;
    }
    else{ // first lap
      if(is_multi)
        iterator = new BiwaScheme.Iterator.ForMulti(obj);
      else
        iterator = BiwaScheme.Iterator.of(obj);
    }

    if(!iterator.has_next()){
      return callbacks["finish"]();
    }
    else{
      x = iterator.next();
      var result = callbacks["call"](x);
      result.after = loop;
      return result;
    }
  }
  return loop(null);
}
BiwaScheme.Call.multi_foreach = function(obj, callbacks){
  return BiwaScheme.Call.foreach(obj, callbacks, true);
}

///
/// Interpreter
///

BiwaScheme.Interpreter = BiwaScheme.Class.create({
  // new BiwaScheme.Interpreter()
  // new BiwaScheme.Interpreter(lastInterpreter)
  // new BiwaScheme.Interpreter(errorHandler)
  // new BiwaScheme.Interpreter(lastInterpreter, errorHandler)
  initialize: function(){
    var last_interpreter = null;
    var on_error = null;
    if (arguments.length == 2) {
      last_interpreter = arguments[0];
      on_error = arguments[1];
    } else if (arguments.length == 1 && arguments[0] instanceof BiwaScheme.Interpreter) {
      last_interpreter = arguments[0];
    } else if (arguments.length == 1 && typeof(arguments[0]) == "function") {
      on_error = arguments[0];
    }

    // Interpreter stack
    this.stack = [];
    // JS function to handle error
    this.on_error = on_error || (last_interpreter ? last_interpreter.on_error : function(e){});
    // JS function to handle result
    this.after_evaluate = function(){};

    // (Variables for stack trace)
    // Name of the last variable read by refer-xx
    this.last_refer = last_interpreter ? last_interpreter.last_refer : null;
    // Call stack (array of last_refer)
    this.call_stack = last_interpreter ? _.clone(last_interpreter.call_stack) : [];
    // Counts number of tail calls (= how many times should we pop call_stack
    // in op_return)
    this.tco_counter = [];
    // Maximum length of call_stack
    // (Note: we should cap call_stack for inifinite loop with recursion)
    this.max_trace_size = last_interpreter ? last_interpreter.max_trace_size : BiwaScheme.max_trace_size;
  },

  inspect: function(){
    return [
      "#<Interpreter: stack size=>",
      this.stack.length, " ",
      "after_evaluate=",
      BiwaScheme.inspect(this.after_evaluate),
      ">"
    ].join("");
  },

  // private
  push: function(x, s){
    this.stack[s] = x;
    return s+1;
  },

  // private
  //s: depth of stack to save
  //ret: saved(copied) stack 
  save_stack: function(s){
    var v = [];
    for(var i=0; i<s; i++){
      v[i] = this.stack[i];
    }
    return { stack: v, last_refer: this.last_refer, call_stack: _.clone(this.call_stack), tco_counter: _.clone(this.tco_counter) };
  },

  // private
  //v: stack array to restore
  //ret: lenght of restored stack
  restore_stack: function(stuff){
    v = stuff.stack;
    var s = v.length;
    for(var i=0; i<s; i++){
      this.stack[i] = v[i];
    }
    this.last_refer = stuff.last_refer;
    this.call_stack = _.clone(stuff.call_stack);
    this.tco_counter = _.clone(stuff.tco_counter);
    return s;
  },

  // private
  //s: depth of stack to save
  //n: number of args(for outer lambda) to remove (= 0 unless tail position)
  //ret: closure array
  continuation: function(s, n){
    // note: implementation of this function for final version doesn't exist in 3imp.pdf..
    var ss = this.push(n, s);
    return this.closure(["refer-local", 0,
                          ["nuate", this.save_stack(ss), 
                          ["return"]]], 
                        0,     //n (number of frees)
                        null,  //s (stack position to get frees)
                        -1);   // dotpos
  },

  // private
  // shift stack 
  // n: number of items to skip (from stack top)
  // m: number of items to shift
  // s: stack pointer (= index of stack top + 1)
  shift_args: function(n, m, s){
    for(var i = n-1; i >= -1; i--){
      this.index_set(s, i+m+1, this.index(s, i));
    }
    return s-m-1;
  },

  index: function(s, i){
    return this.stack[s-i-2];
  },

  // private
  index_set: function(s, i, v){
    this.stack[s-i-2] = v;
  },

  // private
  //ret: [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
  closure: function(body, n, s, dotpos){
    var v = []; //(make-vector n+1+1)
    v[0] = body;
    for(var i=0; i<n; i++)
      v[i+1] = this.index(s, i-1);
    v[n+1] = dotpos;

    v.closure_p = true;

    return v;
  },

  // private
  run_dump_hook: function(a, x, f, c, s) {
    var dumper;
    var state;


    if (this.dumper) {
      dumper = this.dumper;
    }
    else if (BiwaScheme.Interpreter.dumper) {
      dumper = BiwaScheme.Interpreter.dumper;
    }
    else
      return;

    if (dumper) {
      state = {"a":a,
               "f":f,
               "c":c,
               "s":s,
               "x":x,
               "stack":this.stack};
      dumper.dump(state);
    }
  },

  // private
  _execute: function(a, x, f, c, s){
    var ret = null;
    //Console.puts("executing "+x[0]);
    
    while(true){ //x[0] != "halt"){

      this.run_dump_hook(a, x, f, c, s);

      switch(x[0]){
      case "halt":
        return a;
      case "refer-local":
        var n=x[1], x=x[2];
        a = this.index(f, n);
        this.last_refer = "(anon)";
        break;
      case "refer-free":
        var n=x[1], x=x[2];
        a = c[n+1];
        this.last_refer = "(anon)";
        break;
      case "refer-global":
        var sym=x[1], x=x[2];
        if(BiwaScheme.TopEnv.hasOwnProperty(sym))
          var val = BiwaScheme.TopEnv[sym];
        else if(BiwaScheme.CoreEnv.hasOwnProperty(sym))
          var val = BiwaScheme.CoreEnv[sym];
        else
          throw new BiwaScheme.Error("execute: unbound symbol: "+BiwaScheme.inspect(sym));

        a = val;
        this.last_refer = sym || "(anon)";
        break;
      case "indirect":
        var x=x[1];
        a = a[0]; //unboxing
        break;
      case "constant":
        var obj=x[1], x=x[2];
        a = obj;
        this.last_refer = "(anon)";
        break;
      case "close":
        var ox=x;
        var n=ox[1], body=ox[2], x=ox[3], dotpos=ox[4];
        a = this.closure(body, n, s, dotpos);
        s -= n;
        break;
      case "box":
        var n=x[1], x=x[2];
        this.index_set(s, n, [this.index(s, n)]); //boxing
        break;
      case "test":
        var thenc=x[1], elsec=x[2];
        x = ((a!==false) ? thenc : elsec);
        break;
      case "assign-global":
        var name=x[1], x=x[2];
        if(!BiwaScheme.TopEnv.hasOwnProperty(name) &&
           !BiwaScheme.CoreEnv.hasOwnProperty(name))
          throw new BiwaScheme.Error("global variable '"+name+"' is not defined");
        
        BiwaScheme.TopEnv[name] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-local":
        var n=x[1], x=x[2];
        var box = this.index(f, n);
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-free":
        var n=x[1], x=x[2];
        var box = c[n+1];
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "conti":
        var n=x[1], x=x[2];
        a = this.continuation(s, n);
        break;
      case "nuate":
        var stack=x[1], x=x[2];
        s = this.restore_stack(stack);
        break;
      case "frame":
        var ret = x[2];
        x = x[1];
        s = this.push(ret, this.push(f, this.push(c, s)));
        this.tco_counter[this.tco_counter.length] = 0;
        break;
      case "argument":
        var x=x[1];
        s = this.push(a, s);
        break;
      case "shift":
        var n=x[1], x=x[2];

        // the number of arguments in the last call
        var n_args = this.index(s, n);  

        s = this.shift_args(n, n_args, s);
        break;
      case "tco_hinted_apply": // just like a regular apply, except we need to trace the # of TCO calls so we can generate a stacktrace
        this.tco_counter[this.tco_counter.length - 1]++;
        x = ["apply"].concat(_.rest(x));
        break;
      case "apply": //extended: n_args as second argument
        var func = a; //, n_args = x[1];

        // Save stack trace
        this.call_stack.push(this.last_refer);
        if (this.call_stack.length > this.max_trace_size) {
          // Remove old memory if it grows too long
          // Note: this simple way may be inconvenient (e.g. no trace
          // will be shown when an error occurred right after returning
          // from a large function)
          this.call_stack.shift();
        }

        // the number of arguments in the last call is
        // pushed to the stack.
        var n_args = this.index(s, -1);
        if(func instanceof Array){ //closure
          a = func;
          x = func[0];

          // The position of dot in the parameter list.
          var dotpos = func[func.length-1];

          if (dotpos >= 0) {
            // The dot is found
            // ----------------
            // => Process the &rest args: packing the rest args into a list.
            var ls = BiwaScheme.nil;
            for (var i=n_args; --i>=dotpos; ) {
              ls = new BiwaScheme.Pair(this.index(s, i), ls);
            }
            if (dotpos >= n_args) {
              // No rest argument is passed to this closure.
              // However, the closure expects the caller passes the rest argument.
              // In such case this VM prepares an empty list as the rest argument.
              // --------------------------------------------------------------
              // => We extend the stack to put the empty list.
              for(var i = -1; i < n_args; i++){
                this.index_set(s, i-1, this.index(s, i));
              }
              s++;
              // => Update the number of arguments
              this.index_set(s, -1, this.index(s, -1) + 1);  
            }
            this.index_set(s, dotpos, ls);
          }
          f = s;
          c = a;
        }
        else if(func instanceof Function){ // Apply JavaScript function
          // load arguments from stack
          var args = [];
          for(var i=0; i<n_args; i++) 
            args.push(this.index(s, i));

          // invoke the function
          var result = func(args, this);

          if(result instanceof BiwaScheme.Pause){
            // it requested the interpreter to suspend
            var pause = result;
            pause.set_state(this, ["return"], f, c, s);
            pause.ready();
            return pause;
          }
          else if(result instanceof BiwaScheme.Call){
            // it requested the interpreter to call a scheme closure

            //   [frame,
            //     [constant... (args)
            //     [constant, proc
            //     [apply]]]]
            //   [frame,
            //     [constant, after
            //     [apply 1]]]]
            //   x
            var call_after = ["frame",
                               ["argument",
                               ["constant", 1,
                               ["argument",
                               ["constant", result.after,
                               ["apply"]]]]],
                             ["return"]];
            var call_proc = ["constant", result.args.length,
                            ["argument",
                            ["constant", result.proc, 
                            ["apply", result.args.length]]]];
            var push_args = _.inject(result.args, function(opc, arg){
              // (foo 1 2) => first push 2, then 1
              //   [constant 2 ... [constant 1 ... ]
              return ["constant", arg, 
                     ["argument",
                     opc]];
            }, call_proc);
            x = ["frame",
                  push_args,
                call_after]
          }
          else{
            // the JavaScript function returned a normal value
            a = result;
            x = ["return"];
          }
        }
        else{
          // unknown function type
          throw new BiwaScheme.Error(BiwaScheme.inspect(func) + " is not a function");
        }
        break;
      case "return":
        // Pop stack frame
        var n=this.index(s, -1);
        var ss=s-n;
        x = this.index(ss, 0),
        f = this.index(ss, 1),
        c = this.index(ss, 2),
        s = ss-3-1;

        // Pop stack trace (> 1 times if tail calls are done)
        var n_pops = 1 + this.tco_counter[this.tco_counter.length - 1];
        this.call_stack.splice(-n_pops);
        this.tco_counter.pop();
        break;
      default:
        throw new BiwaScheme.Bug("unknown opecode type: "+x[0]);
      }
    }

//      if(ret === null)
//        throw new BiwaScheme.Bug("interpreter exited in unusual way");
//      else
//        return ret;
    return a
  },

  // Compile and evaluate Scheme program
  evaluate: function(str, after_evaluate){
    this.parser = new BiwaScheme.Parser(str);
    this.compiler = new BiwaScheme.Compiler();
    if(after_evaluate) 
      this.after_evaluate = after_evaluate;

    if(BiwaScheme.Debug) Console.puts("executing: " + str);
     
    this.is_top = true;
    this.file_stack = [];

    try{
      return this.resume(false);
    }
    catch(e){
      e.message = e.message + " [" + this.call_stack.join(", ") + "]";
      return this.on_error(e);
    }
  },

  // Resume evaluation
  // (internally used from Interpreter#execute and Pause#resume)
  resume: function(is_resume, a, x, f, c, s){
    var ret = BiwaScheme.undef;

    for(;;){
      if(is_resume){
        ret = this._execute(a, x, f, c, s);
        is_resume = false;
      }
      else{
        if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
        var expr = this.parser.getObject();
        if(expr === BiwaScheme.Parser.EOS) break;

        // expand
        expr = BiwaScheme.Interpreter.expand(expr);

        // compile
        var opc = this.compiler.run(expr);
        //if(BiwaScheme.Debug) Console.p(opc);

        // execute
        ret = this._execute(expr, opc, 0, [], 0);
      }

      if(ret instanceof BiwaScheme.Pause){ //suspend evaluation
        return ret;
      }
    }

    // finished executing all forms
    this.after_evaluate(ret);
    return ret;
  },

  // Invoke a scheme closure
  invoke_closure: function(closure, args){
    args || (args = []);
    var n_args  = args.length;

    var x = ["constant", n_args, ["argument", ["constant", closure, ["apply"]]]]
    for(var i=0; i<n_args; i++)
      x = ["constant", args[i], ["argument", x]]

    return this._execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
  },

  // only compiling (for debug use only)
  compile: function(str){
    var obj = BiwaScheme.Interpreter.read(str);
    var opc = BiwaScheme.Compiler.compile(obj);
    return opc;
  }
});

// Take a string and returns an expression.
BiwaScheme.Interpreter.read = function(str){
  var parser = new BiwaScheme.Parser(str);
  var r      = parser.getObject();
  return (r == BiwaScheme.Parser.EOS)? BiwaScheme.eof: r;
};

// Expand macro calls in a expression recursively.
//
// x - expression
// flag - used internally. do not specify this
//
// @throws {BiwaScheme.Error} when x has syntax error
BiwaScheme.Interpreter.expand = function(x, flag/*optional*/){
  var expand = BiwaScheme.Interpreter.expand;
  flag || (flag = {})
  var ret = null;

  if(x instanceof BiwaScheme.Pair){
    switch(x.car){
    case BiwaScheme.Sym("define"):
      var left = x.cdr.car, exp = x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("define"),
              new BiwaScheme.Pair(left, expand(exp, flag)));
      break;
    case BiwaScheme.Sym("begin"):
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), expand(x.cdr, flag));
      break;
    case BiwaScheme.Sym("quote"):
      ret = x;
      break;
    case BiwaScheme.Sym("lambda"):
      var vars=x.cdr.car, body=x.cdr.cdr;
      ret = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
              new BiwaScheme.Pair(vars, expand(body, flag)));
      break;
    case BiwaScheme.Sym("if"):
      var testc=x.second(), thenc=x.third(), elsec=x.fourth();
      ret = BiwaScheme.List(BiwaScheme.Sym("if"),
                            expand(testc, flag),
                            expand(thenc, flag),
                            expand(elsec, flag));
      break;
    case BiwaScheme.Sym("set!"):
      var v=x.second(), x=x.third();
      ret = BiwaScheme.List(BiwaScheme.Sym("set!"), v, expand(x, flag));
      break;
    case BiwaScheme.Sym("call-with-current-continuation"): 
    case BiwaScheme.Sym("call/cc"): 
      var x=x.second();
      ret = BiwaScheme.List(BiwaScheme.Sym("call/cc"), expand(x, flag));
      break;
    default: //apply
      var transformer = null;
      if(BiwaScheme.isSymbol(x.car)){
        if(BiwaScheme.TopEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.TopEnv[x.car.name];
        else if(BiwaScheme.CoreEnv[x.car.name] instanceof BiwaScheme.Syntax)
          transformer = BiwaScheme.CoreEnv[x.car.name];
      }

      if(transformer){
        flag["modified"] = true;
        ret = transformer.transform(x);

//            // Debug
//            var before = BiwaScheme.to_write(x);
//            var after = BiwaScheme.to_write(ret);
//            if(before != after){
//              console.log("before: " + before)
//              console.log("expand: " + after)
//            }

        var fl;
        for(;;){
          ret = expand(ret, fl={});
          if(!fl["modified"]) 
            break;
        }
      }
      else{
        var expanded_car = expand(x.car, flag);
        var expanded_cdr;
        if(!(x.cdr instanceof BiwaScheme.Pair) && (x.cdr !== BiwaScheme.nil)){
          throw new Error("proper list required for function application "+
                          "or macro use: "+BiwaScheme.to_write(x));
        }
        expanded_cdr = BiwaScheme.array_to_list(
                         _.map(x.cdr.to_array(),
                               function(item){ return expand(item, flag); }));
        ret = new BiwaScheme.Pair(expanded_car, expanded_cdr);
      }
    }
  }
  else{
    ret = x;
  }
  return ret;
};
//
// R7RS Promise (lazy library)
//
BiwaScheme.Promise = BiwaScheme.Class.create({
  initialize : function(done, thunk_or_value){
    this.box = [done, thunk_or_value];
  },

  // Return true when this promise is already calculated
  is_done: function() {
    return this.box[0];
  },

  // Return calculated value of this promise
  value: function() {
    if (!this.is_done()) {
      throw new BiwaScheme.Bug("this promise is not calculated yet");
    }
    return this.box[1];
  },

  thunk: function() {
    if (this.is_done()) {
      throw new BiwaScheme.Bug("this promise does not know the thunk");
    }
    return this.box[1];
  },

  update_with: function(new_promise) {
    this.box[0] = new_promise.box[0];
    this.box[1] = new_promise.box[1];
    new_promise.box = this.box;
  }
});
BiwaScheme.isPromise = function(obj) {
  return (obj instanceof BiwaScheme.Promise);
};

// Create fresh promise
BiwaScheme.Promise.fresh = function(thunk) {
  return new BiwaScheme.Promise(false, thunk);
};
// Create calculated promise
BiwaScheme.Promise.done = function(value) {
  return new BiwaScheme.Promise(true, value);
};
///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
BiwaScheme.check_arity = function(len, min, max){
  var fname = arguments.callee.caller
                ? arguments.callee.caller.fname
                : "(?)";
  if(len < min){
    if(max && max == min)
      throw new BiwaScheme.Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaScheme.Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaScheme.Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
}
BiwaScheme.define_libfunc = function(fname, min, max, func){
  var f = function(ar, intp){
    BiwaScheme.check_arity(ar.length, min, max);
    return func(ar, intp);
  };

  func["fname"] = fname; // for assert_*
  f["fname"]    = fname; // for check_arity
  f["inspect"] = function(){ return this.fname; }
  BiwaScheme.CoreEnv[fname] = f;
}
BiwaScheme.alias_libfunc = function(fname, aliases) {
  if (BiwaScheme.CoreEnv[fname]) {
    if (_.isArray(aliases)) {
      _.map(aliases, function(a) { BiwaScheme.alias_libfunc(fname, a); });
    } else if (_.isString(aliases)) {
      BiwaScheme.CoreEnv[aliases] = BiwaScheme.CoreEnv[fname];
    } else {
      console.error("[BUG] bad alias for library function " +
                    "`" + fname + "': " + aliases.toString());
    }
  } else {
    console.error("[BUG] library function " +
                  "`" + fname + "'" +
                  " does not exist, so can't alias it.");
  }
};
BiwaScheme.define_syntax = function(sname, func) {
  var s = new BiwaScheme.Syntax(sname, func);
  BiwaScheme.CoreEnv[sname] = s;
}
BiwaScheme.define_scmfunc = function(fname, min, max, str){
  (new BiwaScheme.Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//
BiwaScheme.make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname
                  : "";
    check.apply(this, [fname].concat(_.toArray(arguments)));
  }
}
BiwaScheme.make_simple_assert = function(type, test, _fname){
  return BiwaScheme.make_assert(function(fname, obj, opt){
    if(_fname) fname = _fname;
    option = opt ? ("("+opt+")") : ""
    if(!test(obj)){
      throw new BiwaScheme.Error(fname + option + ": " +
                                 type + " required, but got " +
                                 BiwaScheme.to_write(obj));
    }
  })
}

BiwaScheme.assert_number = BiwaScheme.make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof BiwaScheme.Complex);
});

BiwaScheme.assert_integer = BiwaScheme.make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

BiwaScheme.assert_real = BiwaScheme.make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

BiwaScheme.assert_between = BiwaScheme.make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number required, but got " +
                               BiwaScheme.to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number must be between " +
                               from + " and " + to + ", but got " +
                               BiwaScheme.to_write(obj));
  }
});

BiwaScheme.assert_string = BiwaScheme.make_simple_assert("string", _.isString);

BiwaScheme.assert_char = BiwaScheme.make_simple_assert("character", BiwaScheme.isChar);
BiwaScheme.assert_symbol = BiwaScheme.make_simple_assert("symbol", BiwaScheme.isSymbol);
BiwaScheme.assert_port = BiwaScheme.make_simple_assert("port", BiwaScheme.isPort);
BiwaScheme.assert_pair = BiwaScheme.make_simple_assert("pair", BiwaScheme.isPair);
BiwaScheme.assert_list = BiwaScheme.make_simple_assert("list", BiwaScheme.isList);
BiwaScheme.assert_vector = BiwaScheme.make_simple_assert("vector", BiwaScheme.isVector);

BiwaScheme.assert_hashtable = BiwaScheme.make_simple_assert("hashtable",
                                          BiwaScheme.isHashtable);
BiwaScheme.assert_mutable_hashtable = BiwaScheme.make_simple_assert("mutable hashtable",
                                            BiwaScheme.isMutableHashtable);

BiwaScheme.assert_record = BiwaScheme.make_simple_assert("record",
                                          BiwaScheme.isRecord);
BiwaScheme.assert_record_td = BiwaScheme.make_simple_assert("record type descriptor",
                                          BiwaScheme.isRecordTD);
BiwaScheme.assert_record_cd = BiwaScheme.make_simple_assert("record constructor descriptor",
                                          BiwaScheme.isRecordCD);
BiwaScheme.assert_enum_set = BiwaScheme.make_simple_assert("enum_set",
                                          BiwaScheme.isEnumSet);
BiwaScheme.assert_promise = BiwaScheme.make_simple_assert("promise",
                                          BiwaScheme.isPromise);

BiwaScheme.assert_function = BiwaScheme.make_simple_assert("JavaScript function",
                                         _.isFunction);
BiwaScheme.assert_closure = BiwaScheme.make_simple_assert("scheme function",
                                        BiwaScheme.isClosure);
BiwaScheme.assert_procedure = BiwaScheme.make_simple_assert("scheme/js function", function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
});

BiwaScheme.assert_date = BiwaScheme.make_simple_assert("date", function(obj){
  // FIXME: this is not accurate (about cross-frame issue)
  // https://prototype.lighthouseapp.com/projects/8886/tickets/443
  return obj instanceof Date;
});

//var assert_instance_of = BiwaScheme.make_assert(function(fname, type, obj, klass){
//  if(!(obj instanceof klass)){
//    throw new BiwaScheme.Error(fname + ": " +
//                               type + " required, but got " +
//                               BiwaScheme.to_write(obj));
//  }
//});

BiwaScheme.assert = BiwaScheme.make_assert(function(fname, success, message, _fname){
  if(!success){
    throw new BiwaScheme.Error((_fname || fname)+": "+message);
  }
});

//
// deprecation
//

// Show deprecation warnig
// @param {string} title - feature to be deprecated
// @param {string} ver - when it will be removed (eg. "1.0")
// @param {string} alt - alternatives
BiwaScheme.deprecate = function(title, ver, alt){
  if(BiwaScheme.suppress_deprecation_warning) return;

  var msg = title+" is deprecated and will be removed in BiwaScheme "+ver+ ". "+
            "Please use "+alt+" instead";
  console.warn(msg); 
};

//
// utils
//

// Parses a fractional notation in the format: <num>/<denom> (e.g. 3/7, -9/4),
// where <num> is a valid integer notation, and <denom> is a valid notation
// for a positive integer.
//
// Returns a float if the notation is valid, otherwise false.
//
// @param {string} rep - the string representation of the fraction
// @return {float|false}
BiwaScheme.parse_fraction = function(rep) {
  BiwaScheme.assert_string(rep);

  var frac_parts = rep.split('/');

  if (frac_parts.length !== 2)
    return false;

  var num_rep = frac_parts[0];
  var denom_rep = frac_parts[1];

  var num = BiwaScheme.parse_integer(num_rep, 10);
  var denom = BiwaScheme.parse_integer(denom_rep, 10);

  if (num === false || denom === false)
    return false;

  if (denom <= 0)
    return false;

  return num / denom;
};

// Given a string notation of an integer, and the radix, validates the
// notation: returns true if the notation is valid, otherwise false.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {boolean}
BiwaScheme.is_valid_integer_notation = function(rep, rdx) {
  BiwaScheme.assert_string(rep);
  BiwaScheme.assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  var rdx_symbols = '0123456789abcdefghijklmnopqrstuvwxyz';

  var valid_symbols = rdx_symbols.slice(0, rdx);
  var sym_regex = new RegExp('^[+-]?' + '[' + valid_symbols + ']+$', 'ig');

  return sym_regex.test(rep);
};

// Parse an integer. If the integer does not have a valid representation, or
// produces NaN, - false is returned. If the radix is not within [2..36]
// range, false is returned as well.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {integer|false}
BiwaScheme.parse_integer = function(rep, rdx) {
  BiwaScheme.assert_string(rep);
  BiwaScheme.assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  if (!BiwaScheme.is_valid_integer_notation(rep, rdx))
    return false;

  var res = parseInt(rep, rdx);

  if (Number.isNaN(res))
    return false;

  return res;
};

// Given a string notation of a floating-point number in the standard or
// scientific notation, returns true if the notation valid, otherwise false.
//
// For example:
// "1"      -> true
// "1."     -> true
// "1.23"   -> true
// "1e4"    -> true
// "1E4"    -> true
// "1E4.34" -> false
// "e34"    -> false
//
// @param {string} rep - the string representation of the float.
// @return {boolean}
BiwaScheme.is_valid_float_notation = function(rep) {
  BiwaScheme.assert_string(rep);

  var sci_regex = /^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i;
  var fp_regex  = /(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/;

  if (sci_regex.test(rep) || fp_regex.test(rep))
    return true;

  return BiwaScheme.is_valid_integer_notation(rep, 10);
};

// Parse a floating-point number. If the floating-point number does not have a
// valid representation, or produces -Infinity, +Infinity or NaN, - false is
// returned.
//
// @param {string} rep - the string representation of the floating-point value
// @return {float|false}
BiwaScheme.parse_float = function(rep) {
  BiwaScheme.assert_string(rep);

  if (!BiwaScheme.is_valid_float_notation(rep))
    return false;

  var res = new Number(rep).valueOf();

  if (Number.isNaN(res))
    return false;

  if (!Number.isFinite(res))
    return false;

  return res;
};
module.exports = BiwaScheme;
