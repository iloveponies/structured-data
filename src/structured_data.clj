(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1 )))


(defn height[rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1 )))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
   (let [[p1 p2] inner]
         (and (contains-point? outer p1)
              (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [snd (fn [elems] (get elems 1))]
    (map snd collection)))

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
  (let [uniques (seq (set a-seq))]
    (not (= uniques a-seq))))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))


(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))


(defn authors [books]
  (let [authorsets (map :authors books)]
    (apply clojure.set/union authorsets)))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)
        years (cond
                (boolean dyear) (str " (" byear " - " dyear ")")
                (boolean byear) (str " (" byear " - )")
                :else "")]
    (str name years)))


(defn authors->string [authors]
  (clojure.string/join ", " (map author->string authors)))

(defn book->string [book]
  (let [title (:title book)
        author-names (authors->string (:authors book))]
    (str title ", written by " author-names)))

(defn books->string [books]
  (let [book-count (count books)
        book-count-label (cond
                     (= 0 book-count) "No books"
                     (= 1 book-count) "1 book. "
                     :else (str book-count " books. "))
        book-strings (clojure.string/join ". " (map book->string books))]
    (str book-count-label book-strings ".")))



(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book)))  0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
