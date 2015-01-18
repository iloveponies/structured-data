(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let[[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
  (- x2 x1)))

(defn height [rectangle]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 ]
  (- y2 y1)))

(defn square? [rectangle]
  (if(== (width rectangle) (height rectangle))
    true
    false))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [p1 p2] rectangle [x1 y1] p1 [x2 y2] p2 [px py] point]
    (and
     (<= x1 px x2)
     (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2))))

(defn title-length [book] (count (:title book)))

(defn author-count [book] (count (:authors book)))

(defn multiple-authors? [book] (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors-new (conj (:authors book) new-author)]
    (assoc book :authors authors-new)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [lol (fn [x] (count x))]
  (map lol collection)))

(defn second-elements [collection]
  (let [give-second (fn [x] (get x 1))]
  (map give-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (cond
     (contains? a-set elem) (disj a-set elem)
     :else (conj a-set elem)
   ))

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count (set a-seq)) )))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (set (map :name (book :authors))) (author :name)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [
          name (:name author)
          years (str "(" (:birth-year author) " - " (:death-year author) ")")
       ]
    (str name (if(not(nil? (author :birth-year ))) (str " " years)))
))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (str(book :title) ", written by " (authors->string (book :authors))))


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
