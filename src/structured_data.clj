(ns structured-data)

(defn do-a-thing [x]
  (let [plussa (+ x x)]
    (Math/pow plussa plussa)))

(defn spiff [v] (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v] (let [[a b c] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle] (
        let [eka (get rectangle 0) toka (get rectangle 1)]
          (max (- (first eka) (first toka)) (- (- (first eka) (first toka))) )
                          ))

(defn height [rectangle] (
  let [eka (get rectangle 0) toka (get rectangle 1)]
          (max (- (second eka) (second toka)) (- (- (second eka) (second toka))) )
                          ))

(defn square? [rectangle] (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle] (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [eka (get rectangle 0) toka (get rectangle 1)]
    (if (and (<= (first eka) (first point) (first toka)) (<= (second eka) (second point) (second toka)) ) true false
      )  ))

(defn contains-rectangle? [outer inner]
  (if (and (contains-point? outer (first inner)) (contains-point? outer (second inner))  ) true false))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (if (> (author-count book) 1) true false))

(defn add-author [book new-author] (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author] (if (= (:death-year author) nil) true false ))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] (map second collection))

(defn titles [books] (map :title books))


(defn increasing [x y]
  (if (or (<= x y) (= y nil)) true false))

(defn decreasing [x y]
  (if (or (>= x y) (= y nil)) true false))

(defn monotonic? [a-seq] (if
            (or
              (apply = (map increasing a-seq (rest a-seq))) (apply = (map decreasing a-seq (rest a-seq) )) ) true false ))


(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
