(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [the-1st (get v 0)
        the-2nd (get v 2)]
    (if (and the-1st the-2nd)
        (+ the-1st the-2nd)
        '?)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a1 a2 a3] v]
    (if (and a1 a3)
        (+ a1 a3)
        '?)))

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
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[inner-bottom-left inner-top-right] inner]
    (and (contains-point? outer inner-bottom-left)
         (contains-point? outer inner-top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-item (fn [v] (get v 1))]
    (map second-item collection)))

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
  (not (=
         (count a-seq)
         (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union
         (map (fn [book] (book :authors)) books)))

(defn all-author-names [books]
  (set (map (fn [book-authors] (book-authors :name)) (authors books))))

(defn author->string [author]
  (let [name (author :name)
        birth (author :birth-year)
        death (author :death-year)]
        (if (or birth death)
          (apply str [name " (" birth " - " death ")"])
          name)))

(defn authors->string [authors]
  (apply str
         (interpose ", "
           (map author->string authors))))

(defn book->string [book]
  (interpose ", written by ")
    [(:title book) (authors->string (authors [book]))])

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
