(ns structured-data)

(defn do-a-thing [x]
  (let [s (+ x x)]
    (Math/pow s s)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rect]
  (let [[[x1 y1] [x2 y2]] rect]
    (- x2 x1)
    ))

(defn height [rect]
  (let [[[x1 y1] [x2 y2]] rect]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count(:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq)
     (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
    ))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (get author :birth-year)
        death (get author :death-year)
        years (cond death (str " (" birth " - " death \))
                    birth (str " (" birth " - )")
                    :else "")
        ]
    (str name years)))

(defn authors->string [authors]
  (let [authorstrings (map author->string authors)]
    (apply str (interpose ", " authorstrings))))

(defn book->string [book]
  (let [title (get book :title)
        authors (authors->string (get book :authors))]
    (str title ", written by " authors)
    ))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (let [bookstrings (map book->string books)
          bookcount (count books)
          plurality (if (> bookcount 1) \s "")
          countstring (str bookcount " book" plurality ". ")]
      (str countstring (apply str (interpose ". " bookstrings)) \.))))

(defn books-by-author [author books]
  (let [f (fn [b] (has-author? b author))]
    (filter f books)))

(defn author-by-name [name authors]
  (let [f (fn [a] (= (:name a) name))]
    (first (filter f authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
