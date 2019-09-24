(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v ]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> x1 x2)
      (- x1 x2)
      (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (> y1 y2)
      (- y1 y2)
      (- y2 y1))))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[x y] point
    [[x1 y1] [x2 y2]] rectangle]
    (if (<= x1 x x2)
    (if (<= y1 y y2)
      true
      false)
    false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]]  inner]
    (if (contains-point? outer [x1 y1])
      (if (contains-point? outer [x2 y2])
        true
        false)
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [a (:authors book)
        na (conj a new-author)
        nb (assoc book :authors na)]
    nb))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true
   (apply >= a-seq) true
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq))))
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (:name author)
        b (:birth-year author)
        d (:death-year author)]
    (cond
     (not (boolean b)) n
     (not (boolean d)) (str n " (" b " - )")
     :else (str n " (" b " - " d ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [t (:title book)
        a (authors->string (:authors book))]
    (str t ", written by " a)))

(defn books->string [books]
  (let [c (count books)
        b (apply str (interpose ". " (map book->string books)))]
    (cond
     (== c 0) "No books."
     (== c 1) (str "1 book. " b ".")
     :else (str c " books. " b "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
 (let [b (filter (fn [x] (= (:name x) name)) authors)]
   (if (empty? b)
     nil
     (first b))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors(:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
