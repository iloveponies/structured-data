(ns structured-data)


(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow (+ x x) (+ x x))))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))


(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
    (== (width rectangle) (height rectangle)))

(defn area [rectangle]
    (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let[[a b] point
       [[x1 y1] [x2 y2]] rectangle]
    (and (<= x1 a x2) (<= y1 b y2))))

(defn contains-rectangle? [outer inner]
  (let[[point1 point2] inner]
    (and (contains-point? outer point1)
         (contains-point? outer point2))))


(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let[authors (:authors book)
       newAuthors (conj authors new-author)]
    (assoc book :authors newAuthors)
    ))



(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [func (fn [x] (get x 1))]
    (map func collection)))


(defn titles [books]
    (map :title books))



(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
   :else false))




(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem) ))


(defn contains-duplicates? [a-seq]
  (not(=
   (count a-seq)
   (count(set a-seq)))))



(defn old-book->new-book [book]
  (let[setAuthors (set (:authors book))]
    (assoc book :authors setAuthors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
 (let [authors (fn [book] (:authors book))]
   (apply clojure.set/union (map authors books))))

(defn all-author-names [books]
  (set(map :name (authors books))))

  (defn author->string [author]
    (let[name (:name author)
         byear (:birth-year author)
         dyear (:death-year author)
         ]
      (if byear
        (str name " (" byear " - " dyear ")")
        (str name)
        )))


(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookstr (apply str(interpose ". " (map book->string books)))]
    (cond
      (= (count books) 0) (str "No books.")
      (= (count books) 1) (str "1 book. " bookstr ".")
      :else (str (count books) " books. " bookstr ".")
     )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books ))

(defn author-by-name [name authors]
  (let [writer (filter (fn [author] (= name (:name author))) authors)]
    (first writer)
    ))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [asd (set(map alive? (:authors book)))]
    (contains? asd true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
