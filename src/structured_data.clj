(ns structured-data)

(defn do-a-thing [x]
  (let [yhd (+ x x)]
    (Math/pow yhd yhd)
  ))


(defn spiff [v]
  (let [a (get v 0)
    b (get v 2)]
    (+ a b))
  )


(defn cutify [v]
  (conj v "<3")
  )



(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))

  )


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[ x1 y1] [x2 y2]] rectangle]
    (- x2 x1))

  )


(defn height [rectangle]
  (let [[[ x1 y1] [x2 y2]] rectangle]
    (- y2 y1))

  )


(defn square? [rectangle]
  (= (height rectangle) (width rectangle))
  )



(defn area [rectangle]
  (* (height rectangle) (width rectangle)))



(defn contains-point? [rectangle point]
  (let [[[ x1 y1] [x2 y2]] rectangle [x3 y3] point]
    (and (>= x3 x1) (<= x3 x2) (<= y3 y2) (>= y3 y1)))
  )


(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer [[x3 y3] [x4 y4]] inner]
      (and (contains-point? outer [x3 y3]) (contains-point? outer [x4 y4]))
      )
  )


(defn title-length [book]
  (count(get book :title))
  )



(defn author-count [book]
  (count(get book :authors))
  )



(defn multiple-authors? [book]
  (let [auth (get book :authors)
    num (count auth)]
    (if (< 1 num) true false))
 )

(defn add-author [book new-author]
  (let [auth (get book :authors)
    conjed (conj auth new-author)]
    (assoc book :authors conjed))
  )


(defn alive? [author]
  (let [died (get author :death-year)]
    (nil? died))
  )


(defn element-lengths [collection]
  (let [seqq (seq collection)
        munch (fn [x] (count x))]
    (map munch seqq))
  )


(defn second-elements [collection]
  (let [seqq (seq collection)
        munchiez (fn [x] (get x 1))]
    (map munchiez seqq))

  )


(defn titles [books]
  (let [seqq (seq books)
        munchiez (fn [x] (get x :title))]
    (map munchiez seqq))
  )


(defn monotonic? [a-seq]
  (if(apply <= a-seq) true (apply >= a-seq))

  )


(defn stars [n]
  (apply str(repeat n "*"))
  )


(defn toggle [a-set elem]
  (if(contains? a-set elem)(disj a-set elem) (conj a-set elem))
  )



(defn contains-duplicates? [a-seq]
  (let [seq (set a-seq)
        a (count(set seq))
        b (count a-seq)]
    (if(not(= a b))true false))
  )



(defn old-book->new-book [book]
  (let [names (set(get book :authors))]
  (assoc book :authors names))
  )


(defn has-author? [book author]
 (let [names (set(get book :authors))]
   (contains? names author))

)

;(has-author? cities china)

(defn authors [books]
  (let [author-names
         (fn [book] (set (get book :authors)))]
    (set (apply concat (map author-names books)))))

;(authors [cities, wild-seed])

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [name (str(get author :name))
        birth (str(get author :birth-year))
        death (str(get author :death-year))
        all (str name " " "("birth " - " death ")")]
    (if(= "" (str birth))(str name) (str all)))
    )


;(author->string friedman)
;(author->string felleisen)
;(author->string octavia)

(defn authors->string [authors]
  (let [info (fn [author] (author->string author))
    all (str(apply str(interpose ", "(map info authors))))]
    (if(= "" all)(str "") (str all)))
  )

;(authors->string (:authors little-schemer))
;(authors->string #{octavia})
;(authors->string #{})

(defn book->string [book]
  (let [authors (fn [x] (authors->string (get book :authors)))
      title (str(get book :title))
        authors (authors->string (get book :authors))
        all (str title ", written by " authors)]
    (if(= "" all)(str "")(str all))
      )
  )

;;(book->string wild-seed)
;(book->string little-schemer)

(defn books->string [books]
  (let [many (count books)
        manystr (if(<= 2 many)(str many " books. ")(if(= 0 many)(str "No books")(str "1 book. ")))
        book (fn [x] (book->string x))
        all (str manystr(str(apply str(interpose ", "(map book books)))))]
    (str all "."))

  )

;(books->string [])

(defn books-by-author [author books]
   (filter (fn [book] (has-author? book author)) books)

  )

;(books-by-author china books)
;(books-by-author octavia books)

(defn author-by-name [name authors]
  (first(filter (fn [a] (= name (get a :name))) authors))

  )

 (defn living-authors [authors]
   (filter (fn [auth] (alive? auth))authors)

  )

 (defn has-a-living-author? [book]
   (if (first (filter (fn [auth] (alive? auth))(get book :authors))) true false)

  )

 (defn books-by-living-authors [books]
   (filter (fn [booker] (has-a-living-author? booker)) books)

   )

; %________%
