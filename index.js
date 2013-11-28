(function() {
  var fs = require('fs');
  module.exports = function(logFilename, loggedFilename) {
    return function(id, value) {
      var collection;

      try {
        collection = JSON.parse(fs.readFileSync(logFilename));
      } catch (e) {
        collection = [];
      }

      collection.push({
        file: loggedFilename,
        id: id,
        value: value
      });

      fs.writeFileSync(logFilename, JSON.stringify(collection));
    };
  };

}).call(this);
