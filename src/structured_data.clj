(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx))
)


(defn spiff [x]
  (+
    (get x 0)
    (get x 2)
    ))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
)

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
    (if (== (- x2 x1) (- y2 y1))
      true
      false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
         [xp yp] point]
         (and (<= x1 xp x2)
               (<= y1 yp y2))))

(defn leftbottom [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (point x1 y1)))

(defn righttop [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (point x2 y2)))

(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (leftbottom inner))
       (contains-point? outer (righttop inner))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)
        new-book (assoc book :authors new-authors)]
        new-book))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn length-of-one [x]
  (count x))

(defn element-lengths [collection]
  (map length-of-one collection))

(defn second-elements [collection]
         (let [second-one (fn [x] (get x 1))]
         (map second-one collection)))



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
  (assoc book :authors (set(:authors book))))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (let [authors
        (fn [book] (:authors (old-book->new-book book)))]
     (set (apply clojure.set/union (map authors books)))))


(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
       birth-year (:birth-year author)
       death-year (:death-year author)]
       (if (or birth-year death-year)
         (str name " (" birth-year " - " death-year ")")
         name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)
        string-of-book (fn [book] (book->string book))
        books-text (cond
                     (== n 0) "No books"
                     (== n 1) "1 book. "
                     (> n 1) (str n " books. "))]
    (str (apply str books-text (interpose ". " (map string-of-book books))) ".")))

(defn books-by-author [author books]
  (filter
    (fn [x] (has-author? x author)) books))


(defn author-by-name [name authors]
  (first (filter
    (fn [author] (= (:name author) name)) authors)))


(defn living-authors [authors]
  (filter
           (fn [author] (alive? author)) authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter
    (fn [book] (has-a-living-author? book)) books))



; %________%
