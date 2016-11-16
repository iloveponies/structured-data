(ns structured-data)

(defn do-a-thing [x]
  (let [num (+ x x) ]
    (Math/pow num num))
)

(defn spiff [v]
  (let [first (get v 0) third (get v 2) ]
    (+ first third) )
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v ]
   (+ x z))
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (if (= (- x2 x1) (- y2 y1) )
      true
      false
    )
  )
)

(defn area [rectangle]
  (let [ [[x1 y1] [x2 y2]] rectangle ]
    (* (- x2 x1) (- y2 y1))
  )
)

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2] ] rectangle
          [px py]  point]
    (if (and (<= x1 px x2) (<= y1 py y2))
      true
      false
      )
  )
)

(defn contains-rectangle? [outer inner]
  (let [ [[x1 y1] [x2 y2]] outer
         [[ix1 iy1] [ix2 iy2]] inner ]
      (if (and (contains-point? outer (point ix1 iy1))(contains-point? outer (point ix2 iy2)))
        true
        false
    )
  )
)

(defn title-length [book]
  (count (get book :title))
)

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1)
    true
    false
  )
)

(defn add-author [book new-author]
  (let [ authors (get book :authors)
         new-authors (conj authors new-author) ]
    (assoc book :authors new-authors))
  )

(defn alive? [author]
  (if (nil? (get author :death-year))
    true
    false
    )
  )


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ second-element (fn [elem] (get elem 1)) ]
    (map second-element collection)))

(defn titles [books]
  (map :title books))


(defn monotonic? [seq]
  (if (or (apply <= seq) (apply >= seq))
       true
       false ))

(defn stars [num]
  (let [tmp_stars (repeat num "*") ]
    (apply str tmp_stars)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [sequence]
  (if (= (count(set sequence))(count sequence))
    false
    true ))

(defn old-book->new-book [book]
  (let [ uniq-auths (set (:authors book)) ]
    (assoc book :authors uniq-auths)))

(defn has-author? [book author]
  (let [book-authors (set (map :name (:authors book)))
        book-author (:name author)]
    (if (contains? book-authors book-author)
      true
      false)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(defn all-author-names [books]
  (let [ tmp-auths (authors books) ]
    (set (map :name tmp-auths))))


(defn author->string [author] 
  (let [ name (:name author)
	      birth-year (:birth-year author)
	      death-year (:death-year author)]
    (cond
      (and (nil? birth-year) (nil? death-year))
         (str name)      
      (and (not (nil? birth-year)) (nil? death-year))
         (str name " (" birth-year " - )")      
      :else
         (str name " (" birth-year " - " death-year ")"))))

(defn authors->string [authors]
  (let [tmp-authors (map author->string authors)]
    (apply str (interpose ", " tmp-authors))))

(defn book->string [book]
  (let [ title (:title book)
         authors (authors->string (:authors book)) ]
    (str title ", written by " authors)))

; books->string helpers
(defn book-num-helper [num] 
  (cond
    (= num 0) (str "No books.")
    (= num 1) (str "1 book. ")
    :else
    (str num " books. ")))

(defn books-string-helper [books]
  (let [ tmp-books (interpose ". " (map book->string books)) ]
    (str (book-num-helper (count books)) (apply str tmp-books) ".")))


(defn books->string [books]
  ( if (= (count books) 0)
    (book-num-helper 0)
    (books-string-helper books)))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter 
   (fn [author] 
     (if (= (:name author) name)
       true
       false)) authors)))

(defn living-authors [authors]
  (filter 
   (fn [author] 
     (if (nil? (:death-year author))
       true
       false)) authors))
       

(defn has-a-living-author? [book]
  (let [ authors (:authors book) ]
    (if (not (empty? (living-authors authors)))
      true
      false)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
