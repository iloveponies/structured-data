(ns structured-data)

(defn do-a-thing [x]
  (let [x1 (+ x x)]
    (Math/pow x1 x1)))

(defn spiff [v]
  (if (< (count v) 3)
    nill
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
