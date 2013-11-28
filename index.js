exports.__LOG__ = (id) ->
  logFileName = '/tmp/log.json'

  console.log 'log', id
  (value) ->
    console.log 'logging'
    try
      collection = JSON.parse fs.readFileSync logFileName
    catch e
      collection = []

    collection.push {
      file: __filename
      id: id
      value: value
    }

    console.log 'logging', collection
    fs.writeFileSync logFileName, JSON.stringify collection
