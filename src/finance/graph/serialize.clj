(ns finance.graph.serialize)


(defmulti render-nodes
  "Takes the given repo and database entity and returns a sequence of nodes
  containing the rendered data. The first element of the sequence corresponds
  to the given entity."
  (fn render-dispatch
    [repo entity]
    (:data/type entity)))
