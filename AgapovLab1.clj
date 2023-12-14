(defn group-by-type [data criterion-fn threshold]
  (let [result (atom [])
        current-group (atom [])]

    (defn add-to-group [item]
      (if (empty? @current-group)
        (reset! current-group [item])
        (reset! current-group (conj @current-group item))))

    (defn check-type []
      (when (and (seq @current-group) (criterion-fn @current-group threshold))
        (swap! result conj @current-group)
        (reset! current-group [])))

    (doseq [item data]
      (add-to-group item)
      (check-type))

    (when (seq @current-group)
      (swap! result conj @current-group))

    @result))

(defn example-criterion [group threshold]
  (> (apply + group) threshold))

(def data [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
(def threshold-value 20)

(doseq [group (group-by-type data example-criterion threshold-value)]
  (println group))
