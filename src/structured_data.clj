(ns structured-data)

(defn do-a-thing [x]
  (let [xplusx (+ x x)] (Math/pow xplusx xplusx)))

(defn spiff [v]
 (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
 (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (and (= (- x2 x1) (- y2 y1)) true) 
  ))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [xp yp] point] 
  (and (<= x1 xp x2) (<= y1 yp y2) true)))

(defn contains-rectangle? [outer inner]
  (let [[[x1o y1o] [x2o y2o]] outer [[x1i y1i] [x2i y2i]] inner] 
  (and (<= x1o x1i x2o) (<= x1o x2i x2o) (<= y1o y1i y2o) (<= y1o y2i y2o))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
  (let [oldauthors (:authors book)]
   (assoc book :authors (conj oldauthors new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [collection-x] (get collection-x 1))]
   (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= (seq a-seq)) (apply >= (seq a-seq)))
  )

(defn stars [n]
 (apply str (repeat n "*")))

(defn toggle [a-set elem] 
  (cond (contains? a-set elem) 
  (disj a-set elem)
  :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
 (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
 (set (map :name (authors books))))

(defn author->string [author]
  (str (author :name)))

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
