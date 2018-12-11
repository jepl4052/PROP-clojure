(def <> not=)

(defn setColumns [columns list]
  (map #(select-keys % (into [] columns)) list)
)

(defn setConditions [condition list]
 (filter #((second condition) (% (first condition)) (last condition)) list)
)

(defn setSorting [order list]
  (sort #(compare (order %1) (order %2)) list))

(defmacro select [columns _ list _ conditions _ order]
  `(~setSorting ~order (~setConditions ~conditions (~setColumns ~columns ~list)))
)

(def persons `({:id 1 :name "olle" :height 180} {:id 2 :name "anna" :height 175} {:id 3 :name "isak" :height 185} {:id 4 :name "beatrice" :height 160}))

;(macroexpand '(select [:id :name] from persons where [:id > 2] orderby :id))

(select [:id :name :height] from persons where [:id <> 1] orderby :height)
