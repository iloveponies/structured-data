(ns structured-data)

(defn do-a-thing [x]
  (let [p (+ x x)] (Math/pow p p)))

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v] (+ a b))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))
    )
  )

(defn width [rectangle]
    (let [ [[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))
    )
  )


(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
)

(defn area [rectangle]
(* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
(let [ [[x1 y1] [x2 y2]] rectangle [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
(count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
    (assoc book
      :authors
      (conj (:authors book) new-author))
)


(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
(map count collection)
  )


(defn second-elements [collection]
  (map #(get % 1) collection)
   )

(defn titles [books]
(map :title books)
  )

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)
  )
)

(defn stars [n]
(apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
 (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)
  )
)


(defn contains-duplicates? [a-seq]
  (not (=
   (count a-seq)
   (count (set a-seq))
  ))
)

(defn old-book->new-book [book]
(assoc book :authors (set (:authors book))
  )
)

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn author-sets [books]
  (map :authors books)
  )

(defn authors [books]
  (apply clojure.set/union (author-sets books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn year-string [born dead]
  (cond
   dead (
         str " (" born " - " dead ")"
        )
   born (
          str " (" born " - )"
         )
   :else
         ""
  )
)

(defn author->string [author]
  (let [name (:name author) born (:birth-year author) dead (:death-year author)]
    (str name (year-string born dead))
    ))


 (defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
   )

 (defn book->string [book]
  (str (:title book) ", written by ", (authors->string (:authors book)))
   )

 (defn humanize [v word]
   (let [c (count v)]
   (cond
    (= c 1) (str c " " word)
    :else (str c " " word "s")
    )
   )
   )

 (defn books->string-wocount [books]
   (apply str (
               interpose ", " (map book->string books)))
   )

 (defn books->string [books]
   (if (>= 0 (count books))
     "No books."
     (str (humanize books "book") ". " (books->string-wocount books) ".")
   )
  )


 (defn books-by-author [author books]
  (filter #(has-author? % author) books)
   )


(defn author-by-name [name authors]
(first (filter #(= (:name %) name) authors))
  )

 (defn living-authors [authors]
  (filter alive? authors)
   )

 (defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


 (defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
;
