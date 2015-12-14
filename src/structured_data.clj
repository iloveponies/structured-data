(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y) ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[nr1 nr2 nr3] v]
    (+ nr1 nr3)) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)) )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)) )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1))
      true
      false )))

(defn area [rectangle]
  (let [[[x1,y1] [x2,y2]] rectangle]
    (let [dx (- x2 x1) dy (- y2 y1)]
      (* dx dy))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[xp yp] point]
      (if (and (<= x1 xp x2) (<= y1 yp y2))
        true
        false))))

(defn contains-rectangle? [outer inner]
  (let [[[inner-x1 inner-y1][inner-x2 inner-y2]] inner]
    (let [point-one (point inner-x1 inner-y1)
          point-two (point inner-x2 inner-y2)]
      (if (and (contains-point? outer (point inner-x1 inner-y1))
               (contains-point? outer (point inner-x2 inner-y2)))
        true
        false ))))

(defn title-length [book]
  (let [title (:title book)]
    (count title)))

(defn author-count [book]
  (let [authors (:authors book)]
    (count authors)))

(defn multiple-authors? [book]
  (let [authors (count (:authors book))]
    (if (< 1 authors)
      true
      false)))

(defn add-author [book new-author]
  (let [current-authors (:authors book)]
    (def new-authors (conj current-authors new-author))
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true))

(defn element-lengths [collection]
  (defn counter [element]
    (count element))
  (map counter collection))

(defn second-elements [collection]
  (let [bfun (fn [x] (second x))]
    (map bfun collection)))

(defn titles [books]
  (let [title-grab (fn [book] (:title book))]
    (map title-grab books)))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (= (count a-set) (count a-seq))
      false
      true)))

(defn old-book->new-book [book]
  (let [authors-book (:authors book)]
    (let [authors-set (set authors-book)]
      (assoc book :authors (set authors-book)))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (if (contains? authors author)
      true
      false )))

(defn authors [books]
  (let [authors-book
        (fn [book] (:authors book))]
    (apply clojure.set/union (map authors-book books))))

(defn all-author-names [books]
  (let [author-names
        (fn [author] (:name author))]
    (set (map author-names (authors books)))))


(defn author->string [author]
  (cond
     (contains? author :death-year) (let [name (:name author)
                                          birth (:birth-year author)
                                          death (:death-year author)]
                                      (str name " (" birth " - " death ")"))
     (contains? author :birth-year) (let [name (:name author)
                                          birth (:birth-year author)]
                                      (str name " (" birth " - )"))
     :else (:name author) ))

(defn authors->string [authors]
  (let [formatted-authors (map author->string authors)]
    (apply str(interpose ", " formatted-authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (if (not (empty? books))
    (let [book-str (format "%s." (apply str (interpose ", " (map book->string books))))
          book-count-str (if (> (count books) 1)
                           (str (count books) " books. ")
                           "1 book. ")]
      (str book-count-str book-str))
    "No books."))

(defn books-by-author [author books]
  (let [authorize (fn [book]
                    (if (contains? (:authors book) author)
                      true))]
    (filter authorize books)))

(defn author-by-name [name authors]
  (let [match-name (fn [author]
                     (if (= (:name author) name)
                       author ))]
    (first (filter match-name authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [authors (:authors book)
        alive-authors (living-authors authors)]
    (if (< 0 (count alive-authors))
      true
      false)))

(defn books-by-living-authors [books]
  (let [books-w-living-authors (filter has-a-living-author? books)]
    books-w-living-authors))
