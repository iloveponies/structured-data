(ns structured-data)

(defn do-a-thing [x]
  (let [da-thing (+ x x)]
    (Math/pow da-thing da-thing)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let 
    [[[x1 y1] [x2 y2]] rectangle
     [x y] point]
    (and 
      (<= x1 x x2)
      (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let
    [[bl tr] inner]
    (and
      (contains-point? outer bl)
      (contains-point? outer tr))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [alist] 
                 (first (rest alist)))]
    (map second collection)))

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
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (let [authors (fn [book] 
                  (:authors book))]
    (apply clojure.set/union (map :authors books))))
    ;;(clojure.set/union (map authors books))))
;;(clojure.set/union (map authors books))))

(defn all-author-names [books]
  (let [author-names (fn [book] 
                       (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (:name author)
        years (str (:birth-year author)
                   " - "
                   (if (contains? author :death-year) 
                     (:death-year author)))]
    (if (contains? author :birth-year)
      (str name " (" years ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) 
          ", written by " 
          (authors->string (:authors book))
          ))

(defn books->string [books]
  (let [bookCount (count books)
        countStr (if (== bookCount 1)
                   "1 book. "
                   (str bookCount " books. "))]
    (if (== bookCount 0)
      "No books."
      (str countStr 
           (apply str (interpose ", " (map book->string books)))
           "."))
           ))

(defn books-by-author [author books]
  (filter (fn [b] (contains? (:authors b) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter (fn [a] (not (contains? a :death-year))) authors))

(defn has-a-living-author? [book]
  (not (= 0 (count (living-authors (:authors book))))))

(defn books-by-living-authors [books]
  (filter (fn [b] (has-a-living-author? b)) books))

; %________%
