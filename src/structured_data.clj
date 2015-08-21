(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[zeroth first second] v]
    (+ zeroth second)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let[[[x1 y1] [x2 y2]] rectangle
       [x y] point]
     (and (<= x1 x x2) (<= y1 y y2))
       ))

(defn contains-rectangle? [outer inner]
  (let [[lower-left upper-right] inner]
    (and (contains-point? outer lower-left) (contains-point? outer upper-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [original-authors (:authors book)
        new-authors (conj original-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [item] (get item 1)) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond 
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-set (set a-seq)]
    (if (= (count a-seq) (count seq-set)) false true)))

(defn old-book->new-book [book]
  (let [author-list (:authors book)
        author-set (set author-list)]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (let [author-set (:authors book)]
    (contains? author-set author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
    (contains? author :birth-year) (str author-name " (" birth-year " - " death-year ")")
    :else (str author-name))))

(defn authors->string [authors]
  (apply str (interpose ", "(map author->string authors))))

(defn book->string [book]
  (let [book-name (:title book)
        book-authors (:authors book)]
    (str book-name ", written by " (authors->string book-authors))))

(defn books->string [books]
  (let [book-count (count books)]
    (cond (= book-count 0) (str "No books.")
          (= book-count 1) (str "1 book. " (apply str (map book->string books)) ".")
          :else (str book-count " books. " (apply str (map book->string books))))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
