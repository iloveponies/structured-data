(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [frt   (get v 0)
        third (get v 2)]
    (+ frt third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

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
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py]           point]
    (and (<= x1 px x2)
         (<= y1 py y2))))


(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left)
         (contains-point? outer top-right))))


(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [nd (fn [x] (get x 1))]
    (map nd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (empty? a-seq)       true
    (== (count a-seq) 1) true
    (== (first a-seq)
        (second a-seq)) (monotonic? (rest a-seq))
    (< (first a-seq)
       (second a-seq)) (apply <= a-seq)
    :else (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-date (:birth-year author)
        death-date (:death-year author)]
    (if (or birth-date death-date)
      (str author-name " (" birth-date " - " death-date ")")
      author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title   (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [number (count books)
        books-count (fn [x]
                     (cond (< x 1)  "No books."
                           (== x 1) "1 book."
                           :else    (str x " books.")))]
   (if (== number 0)
     (books-count number)
     (str (books-count number)
          " "
          (apply str (interpose ". " (map book->string books)))
          "."))))

(defn books-by-author [author books]
  (filter (fn [x]
            (has-author? x author))
          books))

(defn author-by-name [name authors]
  (first (filter (fn [author]
                   (= (:name author) name))
                 authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (> (count (living-authors authors)) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
