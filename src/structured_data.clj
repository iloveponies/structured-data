(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (height rectangle)
        (width rectangle))))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (and (or (<= x1 x3 x2)
             (>= x1 x3 x2))
         (or (<= y1 y3 y2)
             (>= y1 y3 y2)))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book)
     1))

(defn add-author [book new-author]
  (assoc book
         :authors
         (conj (:authors book)
               new-author)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn element-lengths [collection]
  (map count (seq collection)))

(defn second-elements [collection]
  (map second (seq collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)
       (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [new-authors (set (:authors book))]
    (assoc book
           :authors
           new-authors)))

(defn has-author? [book author]
  (contains? (:authors book)
             author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)]
    (str author-name
         (if (nil? birth-year)
           nil
           (str " ("
                birth-year
                " - "
                death-year
                ")")))))

(defn authors->string [authors]
  (apply str (interpose ", "
                        (map author->string
                             authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book))))

(defn books->string [books]
  (str (cond 
         (= 0 (count books)) "No books."
         (= 1 (count books)) (str "1 book. "
                                  (book->string (first books))
                                  ".")
         :else (str (count books)
                    " books. "
                    (apply str (interpose ". "
                                    (map book->string books)))
                    "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author))
          books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author)))
                 authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author))
          authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book))
          books))

; %________%
