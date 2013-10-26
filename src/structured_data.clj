(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)))

(defn spiff [v]
  (+(get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
    (+ x z))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
 (let [[[x1 y1][x2 y2]] rectangle]
    (== (- y2 y1)(- x2 x1))))

(defn area [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (* (- y2 y1)(- x2 x1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [x3 y3] point]
    (and (<= x1 x3 x2) (<= y1 y3 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x3 y3][x4 y4]] inner]
    (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4] ))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original book
    new (assoc original :authors (conj (:authors original) new-author) )]
    new))

(defn alive? [author]
  (if(contains? author :death-year ) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [apuri (fn [kokoelma] (get kokoelma 1))]
        (map apuri collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or(apply <= a-seq)(apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if(==(count a-seq)(count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [original book
    new (assoc original :authors (set (:authors original)) )]
    new))

(defn has-author? [book author]
  (contains? (:authors book )  author))

(defn authors [books]
  (set(apply concat(map :authors books))))

(defn all-author-names [books]
  (set(map :name (authors books)))  )

(defn author->string [author]
  (let[name (:name author) birth (:birth-year author) death (:death-year author)]
  (if (= nil birth)
    (str name)
    (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
 (let [info (fn [author] (author->string author))
    all (str(apply str(interpose ", "(map info authors))))]
    (if(= "" all)(str "") (str all)))
  )

(defn book->string [book]
  (let [title (:title book) ]
    (str title ", written by " (authors->string (:authors book)) )))

(defn books->string [books]
  (let [number (count books) all (apply str(interpose ". "(map book->string books)))]
    (cond (= "" all) (str "No books.")
    (= number 1) (str number " book. " all ".")
    :else (str number " books. " all "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first(filter (fn [author] (= name (str(:name author)))) authors)))

(defn living-authors [authors]
 (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
 (if (empty? (filter (fn [author] (alive? author)) (:authors book))) false true))

(defn books-by-living-authors [books]
 (filter (fn [book] (has-a-living-author? book)) books))

; %________%
