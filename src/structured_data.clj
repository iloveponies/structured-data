(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

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
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
   (let
    [[[x1 y1] [x2 y2]] rectangle
     [px,  py] point]
     (and
      (<= x1 px x2)
      (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let
    [[[x1 y1] [x2 y2]] inner]
    (and
     (contains-point? outer [x1, y1])
     (contains-point? outer [x1, y2])
     (contains-point? outer [x2, y1])
     (contains-point? outer [x2, y2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc
    book
    :authors
    (conj (:authors book) new-author)))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [f (fn [x] (get x  1))]
  (map f collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (:authors book)) books)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let
    [name (:name author)
    year (if (contains? author :birth-year)
     (let
       [birth (:birth-year author)
       death
        (if
          (contains? author :death-year)
          (:death-year author)
          (str ""))]

       (str " (" birth " - " death ")" )))]

    (str name year)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCountStr
        (cond
         (== 0 (count books)) "No books"
         (== 1 (count books)) "1 book. "
         :else (str (count books) " books. "))
        booksStr (str (apply str (interpose ". " (map book->string books))) ".")]

    (str bookCountStr booksStr)))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


(defn author-by-name [name authors]
  (or nil (first (filter (fn [author] (= name (:name author))) authors))))

(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%