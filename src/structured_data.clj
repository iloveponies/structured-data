(ns structured-data)

(defn do-a-thing [x]
  (let [x-plussed (+ x x)] 
    (Math/pow x-plussed x-plussed )))

(defn spiff [v]
  (cond
   (and (vector? v) (> (count v) 2)) (+ (get v 0) (get v 2))
   :else nil
   ))
  
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
  (let [[[x1][x2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
    (let [[[x1 y1][x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1][x2 y2]] rectangle]
    (and 
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[innerBL innerTR] inner]
    (and
     (contains-point? outer innerBL)
     (contains-point? outer innerTR))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not
   (contains? author :death-year)))
 
(defn element-lengths [collection]
   (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
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
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (let [s (set a-seq)]
    (not
     (== (count a-seq) (count s)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (cond
   (contains? author :birth-year) 
   (str (:name author) " (" (str (:birth-year author)) " - " (str (:death-year author)) ")")
   :else (:name author)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

;; this one's a bit verbose - maybe there is a more elegant solution
(defn books->string [books]
  (let [book-count (count books)
        text-beginning
         (cond 
          (== book-count 0) "No books"
          (== book-count 1) (str book-count " book. ")
          :else (str book-count " books. "))]
    (str text-beginning (apply str (interpose ", " (map book->string books))) ".")))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first 
   (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
