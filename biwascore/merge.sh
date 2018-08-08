#!/bin/sh

# Core
cat core_module_preamble.js src_header/version.js src_header/header.js \
    src_system/class.js \
    src_system/_writer.js \
    src_system/_types.js \
    src_system/error.js \
    "src_system/set.js" \
    src_system/values.js \
    src_system/pair.js \
    src_system/symbol.js \
    src_system/char.js \
    src_system/number.js \
    src_system/port.js \
    src_system/record.js \
    src_system/enumeration.js \
    src_system/hashtable.js \
    src_system/syntax.js \
    src_system/parser.js \
    src_system/compiler.js \
    src_system/pause.js \
    src_system/call.js \
    src_system/interpreter.js \
    src_system/promise.js \
    src_stdlib/infra.js \
    core_module_postamble.js > gen/biwascore.js

# STDLIB
cat stdlib_module_preamble.js src_stdlib/extra_lib.js stdlib_module_postamble.js > gen/extra_lib.js
cat stdlib_module_preamble.js src_stdlib/js_interface.js stdlib_module_postamble.js > gen/js_interface.js
cat stdlib_module_preamble.js src_stdlib/node_functions.js stdlib_module_postamble.js > gen/node_functions.js
cat stdlib_module_preamble.js src_stdlib/r6rs_lib.js stdlib_module_postamble.js > gen/r6rs_lib.js
cat stdlib_module_preamble.js src_stdlib/srfi.js stdlib_module_postamble.js > gen/srfi.js
