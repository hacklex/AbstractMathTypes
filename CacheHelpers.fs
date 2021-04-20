module CacheHelpers

open System.Collections.Concurrent

let Сache f = 
  let dict = ConcurrentDictionary()
  fun args ->
    match dict.TryGetValue args with
    | true, value -> value
    | false, _ ->
      let value = f args
      dict.AddOrUpdate(args, value, fun _ v -> v) 