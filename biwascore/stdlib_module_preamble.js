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
    var assert_port = bs.assert_port;
    var assert_procedure = bs.assert_procedure;
    var assert_string = bs.assert_string;
    var assert_symbol = bs.assert_symbol;
    var assert_vector = bs.assert_vector;
    var assert_list = bs.assert_list;
    var array_to_list = bs.array_to_list;
    var deep_array_to_list = bs.deep_array_to_list;
    var to_write_ss = bs.to_write_ss;
    var to_write = bs.to_write;
    var nil = bs.nil;
    var Pair = bs.Pair;
    var List = bs.List;
    var Sym = bs.Sym;
    var Symbol = bs.Symbol;
    var Call = bs.Call;
    var Console = bs.Console;
    var Compiler = bs.Compiler;
    var Syntax = bs.Syntax;
    var TopEnv = bs.TopEnv;
    var Interpreter = bs.Interpreter;
    var Bug = bs.Bug;

