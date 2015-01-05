(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2))))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (= w h)))

(defn area [rectangle]
   (let [w (width rectangle) 
         h (height rectangle)]
    (* w h)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (>= x2 x x1) (>= y2 y y1))))

(defn contains-rectangle? [outer inner]
  "returns true if inner rectange is inside outer rectangle"
  (let [[[x1 y1] [x2 y2]] outer
        [[x3 y3] [x4 y4]] inner]
    (and (>= x2 x4 x3 x1) (>= y2 y4 y3 y1))))

(defn title-length [book] (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author] 
  (not (contains? author :death-year)))

(defn element-lengths [collection] (map count collection))

(defn second-elements [collection] 
  (map #(get % 1) collection))

(defn titles [books] (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))

(defn stars [n] (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [n (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (if (nil? birth-year) (str n)
     (str n " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books] 
  (if (empty? books) "No books."
    (let [total (count books)
          plural (if (> total 1) " books. " " book. ")]
     (str total plural 
          (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
