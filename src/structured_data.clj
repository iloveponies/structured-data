(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle)(width rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
    (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
  (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
    (let [[point1 point2] inner]
  (and (contains-point? outer point1)(contains-point? outer point2))))

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
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-e (fn [v] (get v 1) )]
  (map second-e collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
  (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
  (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
   (contains? author :death-year)  (let [name (:name author) birth (:birth-year author) death (:death-year author)] (str name " (" birth " - " death ")")) 
   (contains? author :birth-year)  (let [name (:name author) birth (:birth-year author)] (str name " (" birth " - )")) 
  :else (let [name (:name author)] str name)))

(defn authors->string [authors]
  (let [names (map author->string authors)]
  (apply str (interpose ", " names))))

(defn book->string [book]
  (let [authors (authors->string (:authors book))]
  (apply str (:title book) ", written by " authors)))

(defn books->string [books] 
  (let [bstring (apply str (interpose ", " (map book->string books)))]
  (cond 
    (== (count books) 1) (str (count books) " book. " bstring ".") 
    (> (count books) 1) (apply str (count books) " books. " bstring ".") 
    :else (str "No books."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (cond
  (= (:name (first authors)) name) (first authors)
  (= (:name (first (next authors))) name) (first (next authors))
  (= (:name (first (next (next authors)))) name) (first (next (next authors)))
  (= (:name (first (next (next (next authors))))) name) (first (next (next (next authors))))
  :else nil))

(defn living-authors [authors]
 (filter (fn [x] (alive? x)) authors))

(defn has-a-living-author? [book]
  (let [authors ( :authors book)]
  (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
 (filter (fn [x] (has-a-living-author? x)) books))

; %________%
