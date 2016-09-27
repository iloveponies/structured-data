(ns structured-data)

(defn do-a-thing [x]
  (let [two-x (+ x x)]
    (Math/pow two-x two-x)))
(defn spiff [v]
  (+
     (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [x y]]
  (and
       (<= x1 x x2)
       (<= y1 y y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and
       (contains-point? outer p1)
       (contains-point? outer p2)))

(defn title-length [book]
  (count (book :title)))

(defn author-count [book]
  (count (book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book 
    :authors (conj (book :authors) new-author)))

(defn alive? [author]
  (= (author :death-year) nil))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map 
    (fn [c] (get c 1))
    collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
      (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (author :name)
        years (str "(" (author :birth-year) " - " (author :death-year) ")")]
    (if (contains? author :birth-year)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (book :title) ", written by " 
    (authors->string (book :authors))))

(defn books->string [books]
  (let [bs (str (apply str (interpose ". " (map book->string books))) ".")]
    (cond
      (= 0 (count books)) "No books."
      (= 1 (count books)) (str "1 book. " bs)
      :else (str (count books) " books. " bs))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter 
           (fn [author] (= name (:name author)))
           authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (pos? (count (living-authors (book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
