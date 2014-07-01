(ns structured-data
  (:require clojure.set))

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)
    )
  )

(defn spiff [v]
  "Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?
(spiff [1 2 3])       ;=> 4
(spiff [1 2 3 4 5 6]) ;=> 4
(spiff [1 2])         ;=> ?
(spiff [])            ;=> ?"
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

(defn cutify [v]
  "Write the function (cutify v) that takes a vector as a parameter and adds \"<3\" to its end."
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y ] point]
    (if (and
         (<= x1 x x2)
         (<= y1 y y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and
         (contains-point? outer [x1 y1])
         (contains-point? outer [x1 y2])
         (contains-point? outer [x2 y1])
         (contains-point? outer [x2 y2])) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
       (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  "Use map to write the function (second-elements collection) that
takes a vector of vectors and returns a sequence of the second
elements."
  (let [second (fn [x] (get x 1))]
    (map second collection)))

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
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
       (assoc book :authors (set authors))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (let [getauthors (fn [book] (:authors book))]
    (apply clojure.set/union (map getauthors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        year (if (:birth-year author) (str " (" (:birth-year author) " - " (:death-year author) ")") nil) ]
    (str name year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (if (:title book)
    (str (:title book) ", written by " (authors->string (:authors book)))
    nil))

(defn books->string [books]
  (let [n (count books)]
    (cond
     (= 0 n) "No books."
     :else (let [book (if (= 1 n) " book. " " books. ")]
             (str n book (apply str (interpose ". " (map book->string books)) ) ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author) ) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
