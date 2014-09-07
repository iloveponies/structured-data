(ns structured-data)

(defn do-a-thing [x]
  (let [sumx (+ x x)]
    (Math/pow sumx sumx)))

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
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2)
         (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)))

(defn alive? [author]
  (not (:death-year author)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or 
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str
         (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-length (count a-seq)
        set-length (count (set a-seq))]
    (not (== seq-length set-length))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (set (:authors book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years (if birth-year
                (str " (" birth-year " - " death-year ")")
                "")]
    (str name years))) 

(defn authors->string [authors]
  (apply str 
         (interpose ", "
                    (map author->string authors))))

(defn book->string [book]
  (apply str
         (interpose ", "
                    [(:title book)
                    (str "written by " (authors->string (:authors book)))])))

(defn books->string [books]
  (let [all-books (apply str (interpose ". " (map book->string books) ))
        num-of-books (cond 
                      (== (count books) 0) "No books"
                      (== (count books) 1) "1 book. "
                      :else (str (count books) " books. "))]
        (str num-of-books all-books ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter 
          (fn [auth] (= name (:name auth))) 
          authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
