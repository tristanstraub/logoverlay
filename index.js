(function() {
    module.exports = function(logFilename, loggedFilename) {
        var objectToLog;

        return function(id, value) {
            var collection;
            var objectToLog = {
                file: loggedFilename,
                id: id,
                value: value
            };

            if (typeof logFilename === 'function') {
                logFilename(objectToLog);
            } else {
                var fs = require('fs');
                try {
                    collection = JSON.parse(fs.readFileSync(logFilename));
                } catch (e) {
                    collection = [];
                }

                collection.push(objectToLog);
                fs.writeFileSync(logFilename, JSON.stringify(collection));
            }
        };
    };

}).call(this);
