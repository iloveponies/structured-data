(ns structured-data)

(defn do-a-thing [x]
  (let [xPlusX (+ x x)]
  (Math/pow xPlusX xPlusX)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
    )
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
   (if (== (- x2 x1) (- y2 y1)) true false)))


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [xp yp] point]
   (if (and (<= x1 xp x2) (<= y1 yp y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[xo1 yo1] [xo2 yo2]] outer [[xi1 yi1] [xi2 yi2]] inner]
   (if (and (contains-point? outer (point xi1 yi1)) (contains-point? outer (point xi2 yi2))) true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (count (:authors book)) 1) true false))

(defn add-author [book new-author]
  (let [newAuthors (conj (:authors book) new-author)] (assoc book :authors newAuthors)))

(defn alive? [author]
   (if (= (:death-year author) nil) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
  (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
     :else false
   )
  )

(defn stars [n]
  (let [starSeq (repeat n "*")]
  (apply str starSeq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (if (== (count a-seq) (count a-set))
      false
      true
      )
    )
  )

(defn old-book->new-book [book]
  (let [setAuthors (set (:authors book))]
    (assoc book :authors setAuthors)
    )
  )

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false
    )
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (cond
   (contains? author :death-year) (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
   (contains? author :birth-year) (str (:name author) " (" (:birth-year author) " - " ")")
   :else                          (str (:name author))
   )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [booksNro (count books)]
    (cond
     (== booksNro 0) (str "No books.")
     (== booksNro 1) (str booksNro " book. " (book->string (get books 0)) ".")
     :else  (str booksNro " books. " (apply str (interpose ", " (map book->string books))) ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (if (contains? (set (map :name authors)) name)
    (first (filter (fn [x] (= name (:name x)))  authors))
    nil
    )
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [livingAuthors (filter alive? (:authors book)) nroLivingAuthors (count livingAuthors)]
    (if (> nroLivingAuthors 0)
      true
      false)
    )
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
