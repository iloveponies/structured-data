(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

(defn spiff [x]
  (+ (get x 0) (get x 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x2 x1)))

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle))
    true
    false
  ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x3,y3] point]
  (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
  (and (contains-point? outer [x1 y1])
       (contains-point? outer [x2 y2]))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))


(defn multiple-authors? [book]
  (if ( < 1 (author-count book))
    true
    false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not(contains? author :death-year)))

(defn length [x]
  (count x))

(defn element-lengths [collection]
  (map length collection))

(defn second-elements [collection]
  (let [second (fn [collection] (get collection 1))]
  (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (if(> (count a-seq)(count(set a-seq)))
    true
    false))

(defn old-book->new-book [book]
  (let [old (:authors book)
        new (set old)]
      (assoc book :authors new)))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (apply clojure.set/union(map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        dead (:death-year author)]
  (if(contains?  author :birth-year)
    (str name " (" birth " - " dead ")")
    (str name))))

(defn authors->string [authors]
  (apply str(interpose ", "(map author->string authors))))

(defn book->string [book]
  (let[title (:title book)]
    (if(< 0 (count (get book :authors)))
    (str title ", written by "(authors->string (get book :authors)))
    (str title))))

(defn books->string [books]
  (let [size (count books)]
  (if(< 0 size)
    (if(< 1 size)
      (str (apply str size " books. " (interpose ", "(map book->string books)))  ".")
      (str (apply str size " book. " (interpose ", "(map book->string books))) "."))
  (str "No books."))))


(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
