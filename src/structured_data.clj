(ns structured-data)

(defn do-a-thing [x]
  (let [doublex (+ x x)]
    (Math/pow doublex doublex)))

(defn spiff [[x _ z]]
  (+ x z))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (>= x x1)
         (>= y y1)
         (<= x x2)
         (<= y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [{authors :authors} book]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (let [{death-year :death-year} author]
    (not (boolean death-year))))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get2 (fn [c] (get c 1))]
    (map get2 collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [{authors :authors} book]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [{authors :authors} book]
    (contains? authors author)))

(defn authors [books]
  (let [get-authors (fn [book] (:authors book))]
    (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{name :name
         birth-year :birth-year
         death-year :death-year} author
        birth-range (cond
                      death-year (str " (" birth-year " - " death-year ")")
                      birth-year (str " (" birth-year " - )")
                      :else "")]
    (str name birth-range)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [{title :title authors :authors} book]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    :else (let [book-count (if (== (count books) 1) "1 book. " (str (count books) " books. "))]
            (str book-count (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (boolean (some alive? (:authors book))))

; Could also do below, but I think above is more elegant?
;(defn has-a-living-author? [book]
;  (not (empty? (filter alive? (:authors book))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
