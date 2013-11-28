(function() {
  exports.__LOG__ = function(id) {
    var logFileName;
    logFileName = '/tmp/log.json';
    console.log('log', id);
    return function(value) {
      var collection, e;
      try {
        collection = JSON.parse(fs.readFileSync(logFileName));
      } catch (_error) {
        e = _error;
        collection = [];
      }
      collection.push({
        file: __filename,
        id: id,
        value: value
      });
      return fs.writeFileSync(logFileName, JSON.stringify(collection));
    };
  };

}).call(this);
