(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x )]
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
    (- x2 x1)
    ))

(defn height [rectangle]
  (let
    [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
     )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let
    [
     [[x1 y1] [x2 y2]] rectangle
     [p1 p2] point
    ]
    (and (<= y1 p2 y2) (<= x1 p1 x2))
      ))

(defn contains-rectangle? [outer inner]
  (let
    [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])
             (contains-point? outer [x2 y2]))
      ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book))
    )

(defn add-author [book new-author]
 (assoc book :authors
   (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)
    ))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn[x] (get x 1))]
    (map get-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
    )

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq))))
    )

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union
         (map (fn [book] (get book :authors)) books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (if (contains? author :birth-year)
  (str (get author :name)
       " ("
       (get author :birth-year)
       " - "
       (get author :death-year)
       ")")
  (str (get author :name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title)
       ", written by "
       (authors->string (get book :authors))
       ))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   (= 1 (count books)) (str "1 book. "
                            (book->string (first books))
                            ".")
   :else (str (count books)
              " books. "
              (apply str (interpose ". " (map book->string books)))
              ".")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
