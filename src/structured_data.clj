(ns structured-data)

(defn do-a-thing [x]
   (let [y (+ x x)]
     (Math/pow y y))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))



(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]


    (- y2 y1)
    )

  )

(defn square? [rectangle]
  (boolean (= (width rectangle) (height rectangle)))
  )



(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [x y] point]
    (and(<= x1 x x2) (<= y1 y y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]

    (and (contains-point? outer p1) (contains-point? outer p2) )
    )

  )


(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]

  (count (:authors book))
  )

(defn multiple-authors? [book]

  (< 1 (author-count book)) )


(defn add-author [book new-author]
  (let [vanhat (:authors book) uudet (conj vanhat new-author)]

    (assoc book :authors uudet)
    )

  )

(defn alive? [author]

 (not (contains? author :death-year)))

(defn element-lengths [collection]

  (map count collection))

(defn second-elements [collection]
  (let [apu (fn [col] (get col 1))]
  (map apu collection))
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

  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [joukko (set a-seq)]
    (< (count joukko) (count a-seq))
  ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (get book :authors) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [nimi (:name author) syntyma (:birth-year author) kuolema (:death-year author)]

    (if (= syntyma nil) nimi (str nimi " (" syntyma " - " kuolema ")"))


    )

  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [nimi (:title book) kirjoittajat (authors->string (:authors book))]
     (str nimi ", written by " kirjoittajat)
    )
  )

(defn books->string [books]
  (let [lkm (count books) loppuosa (apply str (interpose ", " (map book->string books)) )]

    (if (= lkm 0) "No books." (if (= lkm 1) (str "1 book. " loppuosa "." ) (str lkm " books. " loppuosa "."))
    )

  ))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name))  authors))

  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (boolean (first (filter alive? (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
