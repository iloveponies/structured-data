(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2) ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c)))

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
  (= (height rectangle)(width rectangle) ))

(defn area [rectangle]
  (* (height rectangle)(width rectangle) ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2)  )))

(defn contains-rectangle? [outer inner]
  (let [[[ox1 oy1] [ox2 oy2]] outer [[ix1 iy1] [ix2 iy2]] inner]
    (and (<= ox1 ix1 ix2 ox2) (<= oy1 iy1 iy2 oy2)  )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count(:authors book)) 1 ))

(defn add-author [book new-author]
  (assoc book :authors (conj (book :authors) new-author) ))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (seq (map second collection))))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq ) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*" )))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (=(count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors) ) ))

(defn has-author? [book author]
  (contains? (book :authors) author ))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books)) ))

(defn author->string [author]
  (let [name (if (contains? author :name)
               (:name author)
               (str ""))
       year (if (contains? author :birth-year)
             	 (str " (" (:birth-year author) " - " (if (contains? author :death-year)
                                                      (:death-year author)
                                                      (str ""))")")
		           (str "")
               )]
 (str name year)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) )))

(defn book->string [book]
  (str  (:title book) ", written by " (authors->string(:authors book)) ))

(defn books->string [books]
  (if(= (count books) 0)
    "No books."
    (str (count books) (if(= (count books) 1)
                         " book. "
                         " books. ") (apply str(interpose ", " (map book->string books)) ) "." )))

(defn books-by-author [author books]
  (filter (fn [x]  (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))

; %________%
