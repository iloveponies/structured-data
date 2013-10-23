(ns structured-data)

(defn do-a-thing [x]
  (let
    [xx (+ x x)]
    (Math/pow xx xx)
   ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let
    [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (== (- x1 x2) (- y1 y2))
    ))

(defn area [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
    ))

(defn contains-point? [rectangle point]
  (let
    [[[x1 y1] [x2 y2]] rectangle
     [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))
    ))

(defn contains-rectangle? [outer inner]
  (let
    [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
         (contains-point? outer [x2 y2])
    )))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let
    [x (:authors book)]
    (assoc book :authors (conj x new-author))
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (seq (map count collection)))

(defn second-elements [collection]
  (let
    [seconds (fn [x] (get x 1))]
    (map seconds collection)
    ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
   ))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let
    [x (set (:authors book))]
    (assoc book :authors x)
    ))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let
    [beginning (:name author)]
    (cond
       (:death-year author) (str beginning " (" (:birth-year author) " - " (:death-year author) ")")
       (:birth-year author) (str beginning " (" (:birth-year author) " - )")
       :else (str beginning)
     )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (== (count books) 1) (str "1 book. " (book->string (first books)) ".")
    (< 1 (count books)) (str (count books) " books. " (apply str (interpose ", " (map book->string books))) ".")
    :else (str "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [aut] (= (:name aut) name)) authors)))

(defn living-authors [authors]
  (filter (fn [aut] (alive? aut)) authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
