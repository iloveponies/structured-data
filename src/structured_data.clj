(ns structured-data)


(defn do-a-thing [x]
  (let [doub (+ x x)]
    (Math/pow doub doub)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3") )

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[bl tr] rectangle
        [l b] bl
        [r t] tr
        ]
       (- r l)))

(defn height [rectangle]
  (let [[[l b] [r t]] rectangle]
    (- t b)
    )
  )

(defn square? [rectangle]
  (= (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
   (let [[[l b] [r t]] rectangle
         [x y] point
         ]
     (and (<= l x r) (<= b y t ))))


(defn contains-rectangle? [outer inner]
  (and (contains-point? outer (first inner)) (contains-point? outer (second inner)) ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)) )

(defn multiple-authors? [book]
  (< 1 (author-count book) ))

(defn add-author [book new-author]
  (let [ a (conj (:authors book) new-author )]
    (assoc book :authors a)) )

(defn alive? [author]
  (not(contains? author :death-year )))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  "vec of vecs -> seq of seconds
  "
  (let [helper (fn [s] (get  s 1))]
    (map helper collection)  ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or(apply <= a-seq)(apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n "*"))  
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) 
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count (set a-seq)))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books] 
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
 (set(map :name (authors books))))

(defn author->string [author]
  (let [nm (:name author)
        death (:death-year author)
        year  (if (:birth-year author) 
                (str " (" (:birth-year author) " - " death ")")
                "" ) 
        ]
        (str nm year)))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [ti (:title book)
        au (authors->string (:authors book))
        ]
    (str ti ", written by " au)))

(defn books->string [books]
  (let [nu (count books)
        pl (if (>  nu 1) "s" "")
        bk (apply str(interpose ", " (map book->string books )))
        ]
    (if (empty? books) "No books."
      (str nu " book" pl ". " bk "."   ))))
  
(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)  
  )

(defn author-by-name [name authors]
  (first(filter (fn [au] (= (:name au) name)) authors))
  )

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (let [livecount (count(filter alive? (:authors book)))
        ]
    (> livecount 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
