(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

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
       [x y] point]
  (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
 (let [[[x1 y1] [x2 y2]] inner
       bottomleft (point x1 y1)
       topright (point x2 y2)]
  (and (contains-point? outer bottomleft) (contains-point? outer topright))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [origauthors (:authors book)
        newauthors (conj origauthors new-author)]
    (assoc book :authors newauthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [elementlength (fn [x] (count x))]
  (map elementlength collection)))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
  (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-length (count a-seq)
        set-length (count (set a-seq))]
   (if (== seq-length set-length) false true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors (fn [book] (:authors book))]
    (apply clojure.set/union (map authors books))))

(defn all-author-names [books]
  (let [author-names (fn [author] (:name author))]
  (set (map author-names (authors books)))))

(defn author->string [author]
  (let [name-string (:name author)
        has-birth-year (contains? author :birth-year)
        has-death-year (contains? author :death-year)
        year-string-start (if has-birth-year (str " (" (:birth-year author) " - ") "")
        year-string (if has-death-year (str year-string-start (:death-year author) ")")
                                        (if has-birth-year (str year-string-start ")") ""))
        ]
    (str name-string year-string)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

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
