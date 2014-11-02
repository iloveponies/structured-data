(ns structured-data)

(defn do-a-thing [x]
  (let [y (* 2.0 x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (let [[[x y] [z w]] rectangle]
    (+ z (* x -1))))

(defn height [rectangle]
  (let [[[x y] [z w]] rectangle]
    (+ w (* y -1))))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x y] [z w]] rectangle]
    (if (<= x (get point 0) z) (if (<= y (get point 1) w) true false) false)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (boolean (contains-point? outer point1)) (boolean (contains-point? outer point2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1) true false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
  (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [vector] (get vector 1))]
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
  (if (= (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [death-year (:death-year author) birth-year (:birth-year author) name (:name author)]
    (if death-year (str name " (" birth-year " - " death-year ")") (if birth-year (str name " (" birth-year " - )") name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book) authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (if (> (count books) 0) (str (count books) " book" (if (> (count books) 1) "s. " ". ") (apply str (interpose ". " (map book->string books))) ".") "No books."))

(defn books-by-author [author books]
  (let [has-this-author? (fn [book] (has-author? book author))]
  (filter has-this-author? books)))

(defn author-by-name [name authors]
  (let [is-name? (fn [author] (if (=(:name author) name) true false))]
    (first(filter is-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (if (empty? (living-authors authors)) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
