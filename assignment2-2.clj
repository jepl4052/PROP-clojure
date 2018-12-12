(def <> not=)

(defn setColumns [columns list]
  (map #(select-keys % (into [] columns)) list)
)

(defn setConditions [condition list]
 (filter #((second condition) (% (first condition)) (last condition)) list)
)

(defn setSorting [order list]
  (sort #(compare (order %1) (order %2)) list)
)

(defmacro select [columns _ list _ conditions _ order]
  `(~setSorting ~order (~setConditions ~conditions (~setColumns ~columns ~list)))
)

(defmacro select-2 [columns _ list _ conditions _ order]
  `(sort #(compare (~order %1) (~order %2) )(filter #((second ~conditions) (% (first ~conditions)) (last ~conditions)) (map #(select-keys % (into [] ~columns)) ~list)))
)

(def persons `({:id 1 :name "olle" :height 180} {:id 2 :name "anna" :height 175} {:id 3 :name
"isak" :height 185} {:id 4 :name "beatrice" :height 160}))

;(macroexpand '(select [:id :name] from #{persons} where [:id > 2] orderby :id))

(select [:id :name :height] from persons where [:id <> 1] orderby :height)

(select-2 [:id :name :height] from persons where [:id > 1] orderby :height)

(defn select-3
    [columns _ list _ condition _ order]
    (sort #(compare (order %1) (order %2))
        (filter #((second condition) (% (first condition)) (last condition))
            (map #(select-keys % (into [] columns))
                list
            )
        )
    )
)

(select-3 [:id :name :height] 'from persons 'where [:id <> 2] 'orderby :id)
