(ns structured-data)

(defn do-a-thing [x]
  (let [t (+ x x)] 
    (Math/pow t t))
)

(defn spiff [v]
  (+ (get v 0) (get v 2) )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let
    [[a1 a2 a3] v]
    (+ a1 a3)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
         true 
         false 
    )
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle
        [x3 y3] point]
    (if (or 
          (and (<= x1 x3 x2) (<= y1 y3 y2) ) 
          (and (<= x2 x3 x1) (<= y2 y3 y2) )
          )
      true
      false
        )
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner ]
    (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x2 y2] ) )
      true
      false)
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (= 1 (author-count book) )
    false
    true )
  )

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)
    )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection)
  )

(defn second-elements [collection]
  (let [second-element (fn [x] (get x 1)) ]
    (map second-element collection)
     )
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false
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
  (if (= (count a-seq) (count (set a-seq)))
    false
    true)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (if (contains? authors author )
      true
      false)
    )
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [author-name (:name author)
        birth (str (:birth-year author))
        death (str (:death-year author))]
    (if (empty? birth)
      (str author-name)
      (apply str (list author-name " (" birth " - " death ")"))))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors)))
  )

(defn books->string [books]
  ; requires a re-write!!!! Ugly and not working!!!
    ( let [book-counter (fn [x] (if (= (count books) 1)
                                    (str "1 book.")
                                    (str (count books) " " "books.")) )
           book-texts (fn [x] (apply str (map book->string books)))
           ]
      (if (empty? books)
        (str "No books.")
        (str (book-counter books) " " (book-texts books) ".") 
      )
    )
  )

(defn books-by-author [author books]
  (let [filt (fn [x] (has-author? x author))]
    (filter filt books )
    )
  )

(defn author-by-name [name authors]
  (let [from_name? (fn [x] (= (:name x) name))]
    (first (filter from_name? authors)))
  )

(defn living-authors [authors]
  (let [living-author (fn [x] (alive? x))]
    (filter living-author authors)
    )
  )

(defn has-a-living-author? [book]
  (let [authors (:authors book)] 
    (not (empty? (living-authors authors))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
