 (ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [first (first v)
        third (get v 2)]
    (+ first third)))

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
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp]           point]
    (and (>= x2 xp x1)
         (>= y2 yp y1))))

(defn contains-rectangle? [outer inner]
  (let [inner-bottom-point (first inner) ; what is better? first or get (get inner 0)?
        inner-top-point (get inner 1)] ; I've tried with rest, but rest returns a list.
    (and (contains-point? outer inner-bottom-point)
         (contains-point? outer inner-top-point))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [include-author (conj (:authors book) new-author)]
    (assoc book :authors include-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [coll] (get coll 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
  (let [set-author (set (:authors book))]
    (assoc book :authors set-author)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (let [complete-authors (authors books)]
    (set (map :name complete-authors))))

(defn author->string [author]
  (let [name       (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (cond
      (= birth-year nil) (str name)
      :else (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (let [get-authors (map author->string authors)]
    (apply str (interpose ", " get-authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [counter (count books)
        books-text (apply str (interpose ". " (map book->string books)))]
    (cond
      (= counter 0) (str "No books.")
      (= counter 1) (str "1 book. " (apply book->string books) ".")
      :else (str counter " books. " books-text "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [find-author (fn [author]
                      (= name (:name author)))
        result (filter find-author authors)]
    (if (empty? result)
        nil
        (first result))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
