// biwascore: BiwaScheme fork for app embedding 
//
// See LICENSE.biwascheme for the original license terms

var _ = require("underscore");
var _str = require("underscore.string");

_.str = _str;

module.exports = function(BiwaScheme){
    var bs = BiwaScheme;
    var define_libfunc = bs.define_libfunc;
    var define_syntax = bs.define_syntax;
    var assert_number = bs.assert_number;
    var assert_port = bs.assert_port;
    var assert_function = bs.assert_function;
    var assert_closure = bs.assert_closure;
    var assert_procedure = bs.assert_procedure;
    var assert_string = bs.assert_string;
    var assert_symbol = bs.assert_symbol;
    var assert_vector = bs.assert_vector;
    var assert_list = bs.assert_list;
    var assert_pair = bs.assert_pair;
    var assert_integer = bs.assert_integer;
    var assert_between = bs.assert_between;
    var array_to_list = bs.array_to_list;
    var deep_array_to_list = bs.deep_array_to_list;
    var to_write_ss = bs.to_write_ss;
    var to_write = bs.to_write;
    var to_display = bs.to_display;
    var nil = bs.nil;
    var Pair = bs.Pair;
    var List = bs.List;
    var isList = bs.isList;
    var Sym = bs.Sym;
    var Symbol = bs.Symbol;
    var Call = bs.Call;
    var Console = bs.Console;
    var Port = bs.Port;
    var Complex = bs.Complex;
    var Compiler = bs.Compiler;
    var Syntax = bs.Syntax;
    var TopEnv = bs.TopEnv;
    var CoreEnv = bs.CoreEnv;
    var Interpreter = bs.Interpreter;
    var Bug = bs.Bug;

