(function() {
  fs = require('fs');
  exports.logger = function(logFilename, loggedFilename) {
    return function(id, value) {
      var logFileName;
      return function(value) {
        var collection, e;
        try {
          collection = JSON.parse(fs.readFileSync(logFileName));
        } catch (_error) {
          e = _error;
          collection = [];
        }
        collection.push({
          file: loggedFilename,
          id: id,
          value: value
        });
        return fs.writeFileSync(logFilename, JSON.stringify(collection));
      };
    };
  };

}).call(this);
