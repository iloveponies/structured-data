(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[p1 p2] point]
      (and (<= x1 p1 x2) (<= y1 p2 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer]
    (let [[[a1 b1] [a2 b2]] inner]
      (and (contains-point? outer [a1 b1])
           (contains-point? outer [a2 b2])))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [elem] (count elem)) collection))

(defn second-elements [collection]
  (let [get-second (fn [v] (get v 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book
    :authors
    (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (clojure.set/union (set (apply concat (map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)]
    (let [birth-year (:birth-year author)]
      (let [death-year (:death-year author)]
        (cond
          (not (nil? death-year)) (str name " (" birth-year " - " death-year ")")
          (not (nil? birth-year)) (str name " (" birth-year " - )")
          :else (str name))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
    (empty? books) "No books."
    :else (let [num (count books)]
            (if (== 1 num)
              (str num " book. " (apply str (interpose ". " (map book->string books))) ".")
              (str num " books. " (apply str (interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (filter alive? (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
