(ns structured-data)
(require 'clojure.set)

(defn do-a-thing [x]
  ( let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point
        ]
    (and (<= x1 px x2) (<= y1 py y2))
    ))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (and (contains-point? outer bl) (contains-point? outer tr))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))
    ))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)
    ))

(defn authors [books]
  (let [author-names (fn [book] (:authors (old-book->new-book book)))]
    (apply clojure.set/union (map author-names books))
    ))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (cond
                (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                :else (str)
                )
        ]
    (str name years)
    ))

(defn authors->string [authors]
  (let [names (map author->string authors)]
    (apply str (interpose ", " names))
    ))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-strings (map book->string books)
        formatted (apply str (interpose ". " book-strings))
        prefix (cond
                 (empty? books) "No books"
                 (= 1 (count books)) "1 book. "
                 :else (str (count books) " books. ")
                 )
        ]
    (str prefix formatted ".")
    ))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (let [author (filter (fn [a] (= name (:name a))) authors)]
    (first author)
    ))

(defn living-authors [authors]
  (filter (fn [a] (not (contains? a :death-year))) authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
