(ns aoc-2011.core
  (:require [aoc-2011.day-01 :as day_01])
  (:require [aoc-2011.day-02 :as day_02])
  (:require [aoc-2011.day-03 :as day_03])
  (:require [aoc-2011.day-04 :as day_04])
  (:require [aoc-2011.day-05 :as day_05])
  (:require [aoc-2011.day-06 :as day_06]))

(defn -main
  [& args]
  (print
   [(day_01/a)
    (day_01/b)
    (day_02/a)
    (day_02/b)
    (day_03/a)
    (day_03/b)
    (day_04/a)
    (day_04/b)
    (day_05/a)
    (day_05/b)
    (day_06/a)
    (day_06/b)]))

(-main)
