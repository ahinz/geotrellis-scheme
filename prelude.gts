;; (def list (args)) -> List
;; (def append (list,item)) -> List

(def interval (start, stop, ival)
   (if (> start stop)
      (list ())
      (append 
        (interval (+ start ival) stop ival)
        start)))
