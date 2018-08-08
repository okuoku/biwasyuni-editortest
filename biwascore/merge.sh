#!/bin/sh
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
    core_module_postamble.js > biwascore.js
