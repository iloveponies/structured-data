(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (if (< (count v) 3)
    nil
    (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (< (count v) 3)
    nil
    (let [[x y z] v]
      (+ x z))))

(defn point [x y]
  [x y])


; %________%
