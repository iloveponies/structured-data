(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[left _] [right _]] rectangle]
    (- right left)))

(defn height [rectangle]
  (let [[[_ bottom] [_ top]] rectangle]
    (- top bottom)))

(defn square? [rectangle]
  (let [w (width  rectangle)
        h (height rectangle)]
    (= w h)))

(defn area [rectangle]
  (let [w (width  rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[x y] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[pt1 pt2] inner]
    (and (contains-point? outer pt1)
         (contains-point? outer pt2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [auth1 (:authors book)
        auth2 (conj auth1 new-author)]
    (assoc book :authors auth2)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [xs] (get xs 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq))
        (count a-seq)))

(defn old-book->new-book [book]
  (let [authors# (set (:authors book))]
    (assoc book :authors authors#)))

(defn has-author? [book author]
  (contains? (:authors book)
             author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name  (:name author)
        birth (:birth-year author)
        death (:death-year author)
        years (if birth
                  (str " (" birth " - " death ")")
                  "")]
    (str name years)))

(defn authors->string [authors]
  (apply str
         (interpose ", "
                    (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [books-size (count books)
        numbooks   (if (== 0 books-size)
                       "No books"
                       (if (== 1 books-size)
                           "1 book. "
                           (str books-size " books. ")))
        books-str (map book->string books)]
    (str numbooks (apply str (interpose ". " books-str)) ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))
          books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
