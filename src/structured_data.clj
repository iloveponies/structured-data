(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2  y2]] rectangle]
    (- y2 y1)))
;(println (height (rectangle [1 1] [5 1])))
;(println (height (rectangle [1 1] [5 5])))
;(println (height (rectangle [0 0] [2 3])))

;(println (width (rectangle [1 1] [5 1])))  
;(println (width (rectangle [1 1] [1 1])))
;(println (width (rectangle [3 1] [10 4])))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1))
      true
      false)))
;(println (square? (rectangle [1 1] [2 2])))    ;=> true
;(println (square? (rectangle [1 1] [2 3]))) ;=> false
;(println (square? (rectangle [1 1] [1 1]))) ;=> true
;(println (square? (rectangle [3 2] [1 0]))) ;=> true
;(println (square? (rectangle [3 2] [1 1]))) ;=> false

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))
;(println (area (rectangle [1 1] [5 1]))) 
;(println (area (rectangle [0 0] [1 1]))) 
;(println (area (rectangle [0 0] [4 3])))
;(println (area (rectangle [3 1] [10 4])))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))
;(println (element-lengths ["foo" "bar" "" "quux"])) ;=> (3 3 0 4)
;(println (element-lengths ["x" [:a :b :c] {:y 42}])) ;=> (1 3 1)


(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" byear " - " dyear ")")
      name)))

(defn authors->string [authors]
  (let [m (map author->string authors)
        intm (interpose ", " m)]
    (apply str intm)))

(defn book->string [book]
  (let [authors (authors->string (:authors book))
        book-name (:title book)
        strlist (interpose ", written by " [book-name authors])]
    (apply str strlist)))


(defn books->string [books]
  (if (= 0 (count books))
    "No books."
    (let [book-count (count books)
          book-count-str
          (if (= book-count 1)  " book. "  " books. ")
          book-str (str book-count book-count-str)
          books-list (apply str (interpose ". " (map book->string books)))]
      (str book-str books-list "."))))

(defn books-by-author [author books]
    (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        alive-logical (map alive? authors)]
    (not (empty? (filter (fn [x] (= x true)) alive-logical)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
