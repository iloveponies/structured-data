(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)
     (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- x2 x1)
        (- y2 y1))))

(defn area [rectangle]
  (* (width rectangle)
     (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]

    (and (>= x2 x x1)
         (>= y2 y y1))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x1 y2))
         (contains-point? outer (point x2 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old (:authors book)
        new (conj old new-author)]

    (assoc book :authors new)))

(defn alive? [author]
  (not
   (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection ))

(defn second-elements [collection]
  (let [sec-elem (fn [x] (get x 1))]
    (map sec-elem collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply >= a-seq)
      (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (apply not= (map count [a-seq (set a-seq)])))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
 (set (map :name (authors books))))

(defn author->string [author]
  (let [name  (:name author)
        birth (:birth-year author)
        death (:death-year author)]

    (if birth
      (str name " (" birth " - " death ")")
      (str name))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [part (str (:title book) ", written by ")]

    (str part (authors->string (authors [book])))))

(defn books->string [books]
  (let [n       (count books)
        bookstr (map book->string books)
        prinstr (apply str (interpose ". " bookstr))]

    (cond
     (== n 0) (str "No books.")
     (== n 1) (str n " book. "  prinstr ".")
     :else    (str n " books. " prinstr "."))))

(defn books-by-author [author books]
  (filter
   #(contains? (:authors %) author) books))

(defn author-by-name [name authors]
  (first
   (filter
    #(= (:name %) name) authors)))

(defn living-authors [authors]
  (filter
   #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty?
        (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (concat (filter has-a-living-author? books)))

; %________%
