var pm_tables = require('./node_modules/prosemirror-tables');
var pm_view = require('./node_modules/prosemirror-view');
var pm_state = require('./node_modules/prosemirror-state');

init = function(){
    return pm_state.create({plugins:[pm_tables.tableEditing()]});
};

module.exports = {
    init:init
};
