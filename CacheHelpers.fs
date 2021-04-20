/// A little handy helper to cache any function
/// Thanks Ayrat for sharing this gem!
module CacheHelpers

open System.Collections.Concurrent

let Сache funcToCache = 
  let dictionary = ConcurrentDictionary()
  fun args ->
    match dictionary.TryGetValue args with
    | true, value -> value
    | false, _ ->
      let value = funcToCache args
      dictionary.AddOrUpdate(args, value, fun _ v -> v) 