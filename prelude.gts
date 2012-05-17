(def create-raster (str)
     (scala-lib/create-raster str))

(def radd (n)
     (if (eq n 0)
         0
       (+ n (radd (- n 1)))))
       
(print "Result:" (radd 5))