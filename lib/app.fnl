(global subh (includefnl :lib/subh))

(fn init []
  (clock.run (fn []
               (while true
                 (_G.redraw)
                 (clock.sleep (/ 1 15))))))

(fn redraw []
  (screen.clear)
  (screen.update))

(fn enc [n d])

(fn key [n z])

{: init : redraw : enc : key}

