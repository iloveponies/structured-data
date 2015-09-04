(ns structured-data)

(defn do-a-thing [x]
  :-)

(defn spiff [v]
  :-)

(defn cutify [v]
  :-)

(defn spiff-destructuring [v]
  :-)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  :-)

(defn height [rectangle]
  :-)

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  :-)

(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)

(defn title-length [book]
  (count (get book :title))  
)

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (assoc book :authors (conj authors new-author))
  )  
)

(defn alive? [author]
  (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [second-element (fn [v] (get v 1))]
    (map second-element collection)  
  )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
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
  (> (count a-seq) (count (set a-seq)))
  )

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
    )
  )

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)
    )
  )

(defn authors [books]
  (let [authors-in-book (fn [book] (get book :authors))]
    (apply clojure.set/union (map authors-in-book books))
  )
)

(defn all-author-names [books]
  (let [name-of-author (fn [author] (get author :name))]
    (set (map name-of-author (authors books)))
  )
)

; todo: parempi splittaus 'letin kanssa
(defn author->string [author]
  (apply str(cond
              (contains? author :death-year) (str (get author :name) " (" (get author :birth-year) " - " (get author :death-year) ")")
              (contains? author :birth-year) (str (get author :name) " (" (get author :birth-year) " - )")
              :else                          (str (get author :name))
            )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )


; todo: korjaa koodin tuplaus ..
(defn book->string [book]
  (apply str(cond
              (multiple-authors? book)  (str (get book :title) ", written by " (authors->string (get book :authors)))
              :else                     (str (get book :title) ", written by " (authors->string (get book :authors)))
              )
  )
)

(defn books->string [books]
  (cond
    (== (count books) 0)  (str "No books.")
    )
;  (apply str(interpose ". " (map book->string books)))
  )

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
