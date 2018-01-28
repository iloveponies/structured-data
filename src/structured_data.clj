(ns structured-data)

(defn do-a-thing [x]
  (let [plus (+ x x)]
    (Math/pow plus plus)))

(defn spiff [v]
  (let [fst (get v 0)
        trd (get v 2)]
    (+ fst trd)))

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
  (let [[blx bly] (get rectangle 0)
        [trx try] (get rectangle 1)]
    (- trx blx)))





(defn height [rectangle]
  (let [[blx bly] (get rectangle 0)
        [trx try] (get rectangle 1)]
    (- try bly)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[blx bly] (get rectangle 0)
        [trx try] (get rectangle 1)
        [px py] point]
    (and (<= blx px trx) (<= bly py try))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [with_new_author (conj (:authors book) new-author)]
    (assoc book :authors with_new_author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get_second (fn [x] (get x 1))]
    (map get_second collection)))

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
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [auths (map :authors books)]
   (apply clojure.set/union auths)))


(defn all-author-names [books]
  (set (map :name (authors books))))


(defn a-birth-and-death-years [b-y, d-y]
  (cond
   (nil? b-y) ""
   (nil? d-y) (format " (%s - )" b-y)
   :else (format " (%s - %s)" b-y, d-y))
)

(defn author->string [author]
  (let [a-name (author :name)]
    (let [b-y (author :birth-year)]
      (let [d-y (author :death-year)]
        (format "%s%s" a-name (a-birth-and-death-years b-y d-y))
      )
    )
  )
 )


(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [b-title (book :title)]
    (let [b-strings (authors (list book))]
      (format "%s, written by %s" b-title (authors->string b-strings))
    )
  )
)

(defn books->string [books]
  (let [b-string (apply str(interpose ", " (map book->string books)))]
   (cond
    (= 0 (count books)) "No books."
    (= 1 (count books)) (format "1 book. %s." b-string)
    :else (format "%d books. %s." (count books) b-string))

    )
  )

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books)

  )

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (a :name))) authors))

  )

(defn living-authors [authors]
  (filter alive? authors)

  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
