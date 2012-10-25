(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x skip y] v]
    (+ x y)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    do (== (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    do (and (>= px x1) (<= px x2) (>= py y1) (<= py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    do (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original (:authors book)
        new      (conj original new-author)]
    (assoc book :authors new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn [v] (get v 1))]
    (map sec collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [increases? (fn [x] (apply <= x))
        decreases? (fn [x] (apply >= x))]
    (or (increases? a-seq) (decreases? a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  {:title (:title book)
   :authors (set (:authors book))})

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [names (fn [book] (:authors book))]
    (apply clojure.set/union (map names books))))
  ; (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        b-y  (:birth-year author)
        d-y  (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" b-y " - " d-y ")")
      (str name))))

(defn authors->string [authors]
  (let [strlist (map author->string authors)]
    (apply str (interpose ", " strlist))))

(defn book->string [book]
  (let [name  (:title book)
        alist (authors->string (:authors book))]
    do (str name ", written by " alist)))

(defn books->string [books]
  (let [nbooks (count books)
        sbooks (if (== nbooks 1) " book. " " books. ")
        books (map book->string books)]
     (if (== nbooks 0)
        "No books."
        (str nbooks sbooks (apply str (interpose ". " books)) "."))))

(defn books-by-author [author books]
  (let [has-author (fn [x] (has-author? x author))]
    (filter has-author books)))

(defn author-by-name [name authors]
  (let [find (fn [x] (= name (:name x)))]
    (first (filter find authors))))

(defn living-authors [authors]
; (let [alive? (fn [x] (not (contains? x :death-year)))]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (> (count (living-authors (:authors book))) 0))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
