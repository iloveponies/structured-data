(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [f (get v 0)
        t (get v 2)]
    (+ f t)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle
        w (- x1 x2)]
    (if (< w 0)
      (- w)
      w)))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle
        h (- y1 y2)]
    (if (< h 0)
      (- h)
      h)))

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (= h w)))

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

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
  (let [getter (fn [coll] (get coll 1))]
    (map getter collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (let [f (if (contains? a-set elem)
            disj
            conj)]
    (f a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author
        years (if (nil? birth-year) "" (str " (" birth-year " - " death-year ")"))]
    (str name years)))

(defn authors->string [authors]
  (let [author-strings (map author->string authors)]
    (apply str (interpose ", " author-strings))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (= 0 book-count) "No books."
      (= 1 book-count) (str "1 book. " (book->string (get books 0)) ".")
      :else (str book-count " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
