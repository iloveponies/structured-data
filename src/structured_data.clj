(ns structured-data)

;; Simple isslustration of working with
;; local variables
(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (let [x1 (get v 0)
        x3 (get v 2)]
    (+ x1 x3)))

(defn cutify [v]
  (conj v "<3"))

;; This (destructuring) looks like clojure equivalent of pattern matching
;; from SML. 
(defn spiff-destructuring [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

;; Declaration of point
(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

;; Using destructuring to work with rectangle
;; notice the declaration og the local variable
(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

;; Width and height can be calculated using the
;; coordinates of the rectangle easily
(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

;; Square is simply a rectangle with equal height and width
(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (height rectangle) (width rectangle))
      true
      false)))

;; Area = height * width
(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (<= x1 p1 x2) (<= y1 p2 y2))
      true
      false)))

;; Returns true if the inner rectangle is completely within outer
;; We have to check for the inequality of their coordinates
(defn contains-rectangle? [outer inner]
  (let [[[x1o y1o] [x2o y2o]] outer
        [[x1i y1i] [x2i y2i]] inner]
    (if (and (<= x1o x1i x2i x2o) (<= y1o y1i y2i y2o))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
(> (author-count book) 1))

(defn add-author [book new-author]
  (let [author-list (:authors book)]
    (assoc book :authors (conj author-list new-author))))

(defn alive? [author]
  (if (nil? (:death-year author))
    true
    false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map 
   (fn [list-ele]
     (get list-ele 1))
   collection))

(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or 
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str
         (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count a-seq) (count (set a-seq)))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors
         (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if (nil? birth)
      name
      (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " 
                        (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [book-count (if (= 1 (count books))
                       " book. " " books. ")]
      (str (count books) book-count 
           (apply str (interpose ", " 
                                 (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
