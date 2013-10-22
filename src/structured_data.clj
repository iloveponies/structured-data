(ns structured-data)

(defn do-a-thing [x]
  (let [x x]
    (Math/pow (+ x x) (+ x x)))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[o k e] v]
    (+ o e)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bl1 tr1] [bl2 tr2]] rectangle]
  (- bl2 bl1)))

(defn height [rectangle]
  (let [[[bl1 tr1] [bl2 tr2]] rectangle]
  (- tr2 tr1)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (if (and (<= (get (first rectangle) 0) (get point 0) (get (second rectangle) 0) )
    (<= (get (first rectangle) 1) (get point 1) (get (second rectangle) 1) ) ) true false))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and
      (contains-point? outer inner-bottom-left)
      (contains-point? outer inner-top-right))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors
  (conj (get book :authors) new-author)))

(defn alive? [author]
  (if (get author :death-year)
    false
    true))


(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

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
