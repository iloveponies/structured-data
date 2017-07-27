(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

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
    (if(< x1 x2) (- x2 x1) (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if(< y1 y2) (- y2 y1) (- y1 y2))))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (if(== w h) true false)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (cond
    (and (<= x1 x x2) (<= y1 y y2)) true
    (and (<= x1 x x2) (<= y2 y y1)) true
    (and (<= x2 x x1) (<= y1 y y2)) true
    (and (<= x2 x x1) (<= y2 y y1)) true
    :else             false)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (if(and (contains-point? outer p1) (contains-point? outer p2)) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if(> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [original book
        new (assoc original :authors (conj (:authors original) new-author))]
    new))

(defn alive? [author]
  (if(not (contains? author :death-year)) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secone (fn [x] (get x 1))]
    (map secone collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if(or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if(== (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [x (:authors book)]
    (assoc book :authors (set x))))

(defn has-author? [book author]
  (if(contains? (:authors book) author) true false))

(defn authors [books]
  (let [author-names
         (fn [book] (:authors book))]
    (set (apply clojure.set/union (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [x (:name author)
        y (:birth-year author)
        z (:death-year author)]
    (if(or y z) (str x " " "(" y " " "-" " " z ")") (str x))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [x (:title book)
        y (:authors book)]
    (str x ", written by " (authors->string y))))

(defn books->string [books]
  (let [x (apply str (interpose ". " (map book->string books)))
        y (count books)]
    (cond
      (== y 1) (str y " book. " x ".")
      (> y 1) (str y " books. " x ".")
      :else             "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [x (fn [author] (= name (:name author)))]
    (if(not (empty? (filter x authors))) (first (filter x authors)) nil)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [x (living-authors (:authors book))]
    (if(not (empty?  x)) true false)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
