(ns structured-data)

(defn do-a-thing [x]
  (let [tupla (+ x x)]
    (Math/pow tupla tupla)))

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
  (let [[[x1, y1] [x2, y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1, y1] [x2, y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1, y1] [x2, y2]] rectangle
        [p1, p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1, p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [auths (get book :authors)
        newauths (conj auths new-author)]
    (assoc book :authors newauths)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [getsize (fn [x] (count x))]
    (map getsize collection)))

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [incr (apply <= a-seq)
        decr (apply >= a-seq)]
    (or incr decr)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seqcount (count a-seq)
        setcount (count (set a-seq))]
    (< setcount seqcount)))

(defn old-book->new-book [book]
  (let [auths (get book :authors)]
    (assoc book :authors (set auths))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [auths (fn [book] (get book :authors))]
    (apply clojure.set/union (map auths books))))

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

;;(defn all-author-names [books]
;;  (let [names (fn [auths] (get auths :names))]
;;    (apply clojure.set/union (map :names (authors books))))

(defn author->string [author]
  (let [name (fn [author] (get author :name))
        years (fn [author]
                (if (contains? author :death-year)
                  (str " (" (get author :birth-year) " - " (get author :death-year) ")")
                  (if (contains? author :birth-year)
                    (str " (" (get author :birth-year) " - )"))))]
    (str (name author) (years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (let [booklist (fn [books] (apply str (interpose ". " (map book->string books))))]
    (if (empty? books)
      (str "No books.")
      (if (< (count books) 2)
        (str "1 book. " (booklist books) ".")
        (str (count books) " books. " (booklist books) ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
