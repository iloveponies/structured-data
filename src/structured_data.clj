(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2] :as rectangle] [xp yp :as point]]
  (and (<= x1 xp x2) (<= y1 yp y2)))

(defn contains-rectangle? [outer [p1 p2 :as inner]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

;;;Use assoc and conj to write the function (add-author book new-author) that
;;;takes a book and an author as a parameter and adds author to books authors.
;;;Hint: use let to avoid pain
;;;
;;;(add-author little-schemer {:name "Gerald J. Sussman"})
;;;;=> {:title "The Little Schemer"
;;;;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;;;;              {:name "Matthias Felleisen"}
;;;;              {:name "Gerald J. Sussman"}]}
;;;(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;;;;=> {:authors [{:name "Juhana"} {:name "Jani"}]}
;;;
(defn add-author [book new-author]
  (let [title   (:title book)
        authors (:authors book)]
    (assoc (if title
             (assoc {} :title title)
             {})
           :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [c] (first (rest c))) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq) (apply = a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [birth (:birth-year author)
        death (:death-year author)]
    (str (:name author) (if (or birth death)
                          (str " (" (or birth "") " - " (or death "") ")")
                          ""))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    (= 1 (count books)) (str "1 book. " (book->string (first books)) ".")
    :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn [b] (contains? (:authors b) author)) books))

(defn author-by-name [name authors]
  (let [x (filter (fn [a] (= name (:name a))) authors)]
    (if (nil? x) x
      (first x))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
