(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

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
  (if (= (height rectangle) (width rectangle))
      true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
       (if (and (>= x2 x x1) (>= y2 y y1)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (if (and (contains-point? outer point1)
             (contains-point? outer point2))
      true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors
    (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [print-second
        (fn [element] (get element 1))]
    (map print-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond (apply <= a-seq) true
        (apply >= a-seq) true
        :else            false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(= (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors
    (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors
        (fn [book] (:authors book))]
    (set (apply clojure.set/union (map authors books)))))


(defn all-author-names [books]
  (set (map :name
            (apply clojure.set/union
                   (map :authors books)))))

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        years
        (cond (contains? author :death-year)
                (str " (" birth-year " - " death-year ")")
              (contains? author :birth-year)
                (str " (" birth-year " - )")
              :else "")]
    (str name years)))
(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (let [bookCount (count books)
        bookMap (map book->string books)]
        (cond (= bookCount 0) "No books."
              (= bookCount 1)
                (str "1 book. " (apply str bookMap) ".")
              (> bookCount 1)
                (str bookCount " books. "
                     (apply str
                            (interpose ". " bookMap)) "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [authorList
        (filter (fn [author] (= name (:name author))) authors)]
    (if (= (count authorList) 0) nil (first authorList))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not
   (empty?
    (filter
     (fn [author] (alive? author))
     (authors [book])))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
