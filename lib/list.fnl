(fn repeat [val n]
  "Duplicate the given value N times"
  (let [buf []]
    (for [_ 1 n]
      (table.insert buf val))
    buf))

(fn repeat-with [f n]
  "Create a list of values by calling the given function N times"
  (let [buf []]
    (for [i 1 n]
      (table.insert buf (f i)))
    buf))

(fn extend [left right]
  "Insert all values in the right list into the left list"
  (each [_ val (ipairs right)]
    (table.insert left val)))

{: repeat : repeat-with : extend}

