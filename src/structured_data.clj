(ns structured-data)

(defn do-a-thing [x]
  (let [twice (+ x x)]
    (Math/pow twice twice)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
 (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false)
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (and (<= x1 (get point 0) x2) (<= y1 (get point 1) y2))
      true
      false)
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[bottom-left-inner top-right-inner] inner]
    (if (and (contains-point? outer bottom-left-inner) (contains-point? outer top-right-inner))
      true
      false)
    )
  )

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author)))
  )

(defn alive? [author]
  (if (contains? author :death-year)
    false
    true)
  )

(defn count-fn [vect]
  (count vect)
  )

(defn element-lengths [collection]
  (map count-fn collection))

(defn second-elements [collection]
  (let [pick-second (fn [vect] (get vect 1))]
    (map pick-second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [increasing (fn [vect] (apply <= vect))
        decreasing (fn [vect] (apply >= vect))]
    (if (or (increasing a-seq) (decreasing a-seq))
      true
      false
      )
    )
  )
  

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (< (count (set a-seq)) (count a-seq) )
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books)))
  )

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [author-name (fn [auth] (:name auth))
        author-years (fn [auth] 
                       (if (contains?  auth :birth-year)
                         (str " (" (:birth-year auth) " - " (:death-year auth) ")")
                         ""))]
     (str  (author-name author) (author-years author)))
  )

(defn authors->string [authors]
 (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
   (apply str (interpose ", written by " (concat [(:title book)] [(authors->string (:authors book))])))
  )
(defn books->string [books]
  (let [book-count (fn [vect] (if (> 1 (count vect))
                                "No books"
                                (if (== 1 (count vect))
                                  "1 book"
                                  (str (count vect) " books"))))
        book-names (fn [vect] (if (< 0 (count vect))
                               (str ". " (apply str (interpose ". " (map book->string books))) ".")
                                "."))]
    (str (book-count books) (book-names books))
  )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (let [found (filter (fn [author] (if (= (:name author) name)
                         true
                         false))
          authors)] 
  (if (< 0 (count found))
    (first found)
    nil
    )
  )
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book)))
    false
    true)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
