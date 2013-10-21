(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [[x y z] v]
       (+ x z)) )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle) ) )

(defn contains-point? [rectangle point]
   (let [ [a b] point[[x1 y1][x2 y2]] rectangle]
     (and (<= x1 a x2) (<= y1 b y2)) ))

(defn contains-rectangle? [outer inner]
  (let [ [p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2) )))

(defn title-length [book]
  (count (:title book)) )

(defn author-count [book]
  (count (:authors book)) )

(defn multiple-authors? [book]
  (if (>=(author-count book) 2) true false))

(defn add-author [book new-author]
  (let [origAuthors (:authors book)
        updatedAuthors (conj origAuthors new-author)]
     (assoc book :authors updatedAuthors )
       ) )

(defn alive? [author]
  (not(contains? author :death-year)) )

(defn element-lengths [collection]
  (map (fn derp [x] (count x)) collection))

(defn second-elements [collection]
   (let [
         funks(fn [unts] (get unts 1))]
             (map funks collection))
            )

(defn titles [books]
  (map :title books) )

(defn monotonic? [a-seq]
   (or (apply >= a-seq) (apply <= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)(conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq))
    (count a-seq)))
  )


(defn old-book->new-book [book]
  (let [authorSet (get book :authors)]
  (assoc book :authors (set authorSet) ))
)

(defn has-author? [book author]
   (contains? (book :authors) author ) )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [name (get author :name)
        byear (get author :birth-year)
        dyear (get author :death-year)]

    (if byear (str name " " "("byear" - "dyear")") (str name)))

  )

(defn authors->string [authors]
   (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bname (get book :title)
        bauthors (authors->string (get book :authors))]
    (str bname ", written by " bauthors) )
  )

(defn books->string [books]
  (cond
   (< 1 (count books)) (str (str (count books)" books. ")(apply str(interpose ", " (map book->string books)))".")
   (== 1 (count books)) (str "1 book. " (book->string (first books))".")
   :else "No books."
   )
 )

(defn books-by-author [author books]
   (filter (fn [book] (has-author? book author)) books) )

(defn author-by-name [name authors]
  (first(filter (fn [a] (= name (get a :name))) authors)))

(defn living-authors [authors]
  (filter (fn [auth] (alive? auth))authors))

(defn has-a-living-author? [book]
  (if (first (filter (fn [auth] (alive? auth))(get book :authors))) true false))

(defn books-by-living-authors [books]
  (filter (fn [bookie] (has-a-living-author? bookie)) books))

; %________%
