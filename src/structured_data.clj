(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
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
  (== (width rectangle)
      (height rectangle)))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[px py] point
        [[x1 y1] [x2 y2]] rectangle]
    (and (<= (min x1 x2) px (max x1 x2))
         (<= (min y1 y2) py (max y1 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-right inner-top-left] inner]
    (and (contains-point? outer inner-bottom-right)
         (contains-point? outer inner-top-left))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
    (assoc book
           :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getter (fn [v] (get v 1))]
    (map getter collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
          (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors
         (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union
         (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (or (:death-year author) "")]
    (if birth
      (str name " (" birth " - " death ")")
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [authors (:authors book)]
    (str (:title book) ", written by " (authors->string authors))))

(defn books->string [books]
  (case (count books)
    0 "No books."
    1 (str "1 book. " (book->string (get books 0)) ".")
    (str (count books) " books. "
         (apply str (interpose ". " (map book->string books)))
         ".")))


(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (set (filter alive? authors)))

(defn has-a-living-author? [book]
  (boolean (some alive? (:authors book))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
