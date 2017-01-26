(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))


(defn spiff [v]
  (let [a (get v 0)
        b (get v 2)]
    (+ a b)))

;(spiff [1 2])   NullPointerException in case of vector of size < 3

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
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

;i defined it here
(defn abs [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (abs (- x2 x1)) (abs (- y2 y1)))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (* (- y2 y1) (- x2 x1)))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
        true
        false)))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3] [x4 y4]] inner]
    (if (and (contains-point? outer [x3 y3])
             (contains-point? outer [x4 y4]))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [pickup (fn [x] (get x 1))]
    (map pickup collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*) ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq))
    false
    true))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author )
    true
    false))

(defn authors [books]
  (set (apply concat (map :authors books))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [findyear (fn [x] (if (:birth-year x)
                           (str (:name x) " (" (:birth-year x) " - " (:death-year x) ")")
                           (:name x)))]
    (findyear author)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-name (:title book) writers (:authors book)]
    (str book-name ", written by " (authors->string writers ))))

(defn books->string [books]
  (let [counter (count books) description (apply str(interpose ". "(map book->string books)))]
    (if (== counter 0)
      ( str "No" " books." )
      (if (== counter 1)
        (str counter " book. "  description ".")
        (str counter " books. "  description ".")))))

(defn books-by-author [author books]
  (filter (fn [book] (if (contains? (:authors book) author )
                              true
                              false))
          books))

(defn author-by-name [name authors]
  (let [find-match (fn [details] (if (= (:name details) name)
                                   true
                                   false))]
    (first (filter find-match authors))))

(defn living-authors [authors]
   (let [alive-check (fn [author] (not (contains? author :death-year)))]
     (filter alive-check authors)))

(defn has-a-living-author? [book]
  (let [all-authors (:authors book)]
    (not (empty? (living-authors all-authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
