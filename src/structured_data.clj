(ns structured-data)

(defn boolean [x]
  (if (= x nil) false
    (if (= x false) false true)))

(defn do-a-thing [x]
  (let [thing (+ x x)]
    (Math/pow thing thing)))

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
    (+ (- 0 x1) x2)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (+ (- 0 y1) y2)))

(defn square? [rectangle]
  (if (= (height rectangle) (width rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
     (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer
        [[xi1 yi1] [xi2 yi2]] inner]
   (and (<= xo1 xi1 xi2 xo2) (<= yo1 yi1 yi2 yo2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [original-authors  (:authors book)]
     (assoc book :authors (conj original-authors new-author))))

(defn alive? [author]
  (= (:death-year author) nil))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))
;exercise 20 next up!
(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [original book]
    (assoc book :authors
      (set (:authors original)))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
   (set (clojure.set/union (apply concat (map :authors books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
          years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (str name (if (:birth-year author) years))))

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCount (count books)]
   (if (= bookCount 0) "No books."
     (if (= bookCount 1) (str bookCount " book. " (apply str (map book->string books)) ".")
       (str bookCount " books. " (apply str (interpose ". " (map book->string books)))".")))))

(defn books-by-author [author books]
  (filter (fn [x] (contains? (:authors x) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter (fn [x] (= (:death-year x) nil)) authors))

(defn has-a-living-author? [book]
   (not (empty? (filter (fn [x] (= (:death-year x) nil)) (:authors book)))))


(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books))


; %________%
