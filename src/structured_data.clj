(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)] (Math/pow twox twox)) )

(defn spiff [v]
  (+ (get v 0) (get v 2)) )

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z) )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- x2 x1))
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (- y2 y1))
)

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
)

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [x y] point]
  (and (<= x1 x x2) (<= y1 y y2)))
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[x3 y3] [x4 y4]] inner]
  (and (<= x1 x3 x2) (<= y1 y3 y2) (<= y1 y4 y2) (<= x1 x4 x2)))
)

(defn title-length [book]
  (count (:title book)) )

(defn author-count [book]
  (count (:authors book)) )

(defn multiple-authors? [book]
  (< 1 (count (:authors book))) )

(defn add-author [book new-author]
  (let [originalAuthors (:authors book)
        newAuthors (conj originalAuthors new-author)]
  (assoc book :authors newAuthors))
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  ; tähän jätetty kommentoituna "raaka" tapa hoitaa sama asia!!
 ; (let [firstCollection (get collection 0)
       ; secondCollection (get collection 1)
      ;  thirdCollection (get collection 2)
     ;   firstCollectionSecondElement (get firstCollection 1)
    ;    secondCollectionSecondElement (get secondCollection 1)
   ;     thirdCollectionSecondElement (get thirdCollection 1)]
  ;(seq[firstCollectionSecondElement secondCollectionSecondElement thirdCollectionSecondElement]))
  (let [seconds (fn [coll] (get coll 1))]
    (map seconds collection))
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
)

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
    (set (apply clojure.set/union (map :authors books)))
)

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books))))
)

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
  (if (not (contains? author :birth-year)) name (str name " (" birth-year " - " death-year ")")))
)

(defn authors->string [authors]
  (let [formatted-authors (map author->string authors)]
  (apply str (interpose ", " formatted-authors)))
)

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
  (str title ", written by " authors))
)

(defn books->string [books]
  (let [formatted-books (map book->string books)]
    (cond
    (empty? books) "No books."
    (= (count books) 1) (str (count books) " book. " (apply str (interpose ", " formatted-books)) ".")
    :else (str (count books) " books. " (apply str (interpose ", " formatted-books)) ".")))
)

(defn books-by-author [author books]
  (let [check-author (fn [book] (has-author? book author))] (filter check-author books)))

(defn author-by-name [name authors]
  (let [check-name (fn [author] (= name (:name author)))]
    (first (filter check-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________% :)
