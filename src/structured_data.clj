(ns structured-data)

(defn do-a-thing [x]
  (let [BADAM x]
  (Math/pow (+ x x) (+ x x))))

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
  (- (get (get rectangle 1) 0) (get (get rectangle 0) 0)))

(defn height [rectangle]
  (- (get (get rectangle 1) 1) (get (get rectangle 0) 1)))

(defn square? [rectangle]
  (cond
    (== (height rectangle) (width rectangle)) true
    :else false
    ))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (if (and (<= (get (get rectangle 0) 0) (get point 0) (get (get rectangle 1) 0))
           (<= (get (get rectangle 0) 1) (get point 1) (get (get rectangle 1) 1))
           ) true false))

(defn contains-rectangle? [outer inner]
  (if (and (>= (get (get inner 0) 0) (get (get outer 0) 0))
           (<= (get (get inner 1) 1) (get (get outer 1) 1))
           (>= (get (get inner 0) 1) (get (get outer 0) 1))
           (<= (get (get inner 1) 0) (get (get outer 1) 0))
           ) true false))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
    (let [aikaisemmat (get book :authors)]
      (assoc book :authors(conj aikaisemmat new-author))))

(defn alive? [author]
  (if (= (get author :death-year) nil) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [toka (fn [collection] (get collection 1))]
    (map toka collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false
   ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count (set a-seq)) (count a-seq)) false true))

(defn old-book->new-book [book]
   (let [authorit (set (:authors book))]
     (assoc book :authors authorit)))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
    (let [author-names
         (fn [book] (map :name (authors books)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (if (:death-year author) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
    (if (:birth-year author) (str (:name author) " (" (:birth-year author) " - )")
      (str (:name author)))))

(defn authors->string [authors]
 	(apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
   (== (count books) 0) "No books."
   (== (count books) 1) (str "1 book. " (book->string (first books)) ".")
   (> (count books) 1) (str (count books) " books. " (apply str (interpose ". " (map book->string books)))".")
   ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))