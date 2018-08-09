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

//
// Library functions only work on Node.js
// see also: test/node_functions.js
//

//(function(){
  //if(BiwaScheme.on_node){
  var node = BiwaScheme.node_apis;
/*
    var node = {
      fs: require('fs'),
      path: require('path'),
      process: process
    };
*/
  //}

  // Defines library functions which only works on Node.
  // - On Node: same as define_libfunc
  // - On Browser: defines a stub libfunc which just raises Error
  var define_node_libfunc = function(/*arguments*/){
    var args = _.toArray(arguments);

    if(BiwaScheme.on_node){
      BiwaScheme.define_libfunc.apply(null, args);
    }
    else{
      var func_name = args[0];
      var func = function(ar){
        throw new BiwaScheme.Error("the function '"+func_name+"' "+
          "is not supported in the browser "+
          "(works only on Node.js).");
      };
      args.pop();
      args.push(func);
      BiwaScheme.define_libfunc.apply(null, args);
    }
  };

  //
  // Chapter 9 File System
  //

  //(file-exists? filename)    procedure 
  define_node_libfunc("file-exists?", 1, 1, function(ar){
    BiwaScheme.assert_string(ar[0]);
    return node.fs.existsSync(ar[0]);
  });

  //(delete-file filename)    procedure 
  define_node_libfunc("delete-file", 1, 1, function(ar){
    BiwaScheme.assert_string(ar[0]);
    node.fs.unlinkSync(ar[0]);
    return BiwaScheme.undef;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
  
  //(command-line)    procedure
  define_node_libfunc("command-line", 0, 0, function(ar){
    return BiwaScheme.List.apply(null, node.process.argv);
  });

  //(exit)    procedure 
  //(exit obj)    procedure
  define_node_libfunc("exit", 0, 1, function(ar){
    var obj = ar[0];
    var code = _.isUndefined(obj) ? 0 :
               (obj === false)    ? 1 :
               Number(obj);

    node.process.exit(code);
  });

  //
  // srfi-98 (get-environment-variable)
  //

  // (get-environment-variable name) -> string or #f
  define_node_libfunc("get-environment-variable", 1, 1, function(ar){
    BiwaScheme.assert_string(ar[0]);
    var val = node.process.env[ar[0]];
    return _.isUndefined(val) ? false : val;
  });

  // (get-environment-variables) -> alist of string (("key" . "value"))
  define_node_libfunc("get-environment-variables", 0, 0, function(ar){
    return BiwaScheme.js_obj_to_alist(node.process.env);
  });

//})();
};
