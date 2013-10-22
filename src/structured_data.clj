(ns structured-data)

(defn do-a-thing [x]
  (let [kapow (+ x x)]
    (Math/pow kapow kapow)
 ))

(defn spiff [v]
  (let [x1 (get v 0)
        x2 (get v 2)]
   (+ x1 x2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
       [px py] point]
       (and (<= x1 px x2) (<= y1 py y2)))
    )

(defn contains-rectangle? [outer inner]
  (let [[lu rd] inner]
    (and (contains-point? outer lu) (contains-point? outer rd))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [auth (:authors book)
        fixedbook (assoc book :authors (conj auth new-author))]
    fixedbook))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or(apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        new-book (assoc book :authors (set authors))]
    new-book))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set(map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        year (if (contains? author :birth-year)
               (str " (" (:birth-year author) " - " (:death-year author) ")")
               "")]
    (str name year)))

(defn authors->string [authors]
  (apply str (interpose ","(map author->string authors))))

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
