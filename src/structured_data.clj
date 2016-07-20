(ns structured-data)

(defn do-a-thing [x]
  (let [dblX (+ x x)]
    (Math/pow dblX dblX)))

(defn spiff [v]
      (+ (get v 0)(get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

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
  (if (= (width rectangle) (height rectangle))
    true
    false
    ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (<= x1 xp x2) (<= y1 yp y2))
      true
      false
      )))

(defn contains-rectangle? [outer inner]
  (let [[innerp1 innerp2] inner]
    (and (contains-point? outer innerp1)
             (contains-point? outer innerp2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))


(defn add-author [book new-author]
  (let [authorlist (get book :authors)]
    (assoc book :authors (conj authorlist new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getsec (fn [x] (get x 1))]
    (map getsec collection)))

(defn titles [books]
  (let [gettitle (fn [x] (:title x))]
    (map gettitle books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq)
              (count (set a-seq)))))


(defn old-book->new-book [book]
  ;(assoc book :authors (set (get book :authors))))
  (assoc book :authors (set (get book :authors))))
  ;(get book :authors))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [get-authors (fn [book] (get book :authors))]
    (apply clojure.set/union (map get-authors books))
    ))
(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (set (apply concat (map author-names books))))

(defn author->string [author]
  (str (:name author)
       (if (contains? author :birth-year)
         (str " (" (:birth-year author) " - " (:death-year author) ")")
         ""
         )
       ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (get book :authors))))

(defn books->string [books]
  (let [num-of-books (count books)]
    (if (= num-of-books 0)
    "No books."
    (if (> num-of-books 1)
      (str num-of-books " books. "
           (apply str (interpose ". " (map book->string books))) "."
           )
      (str num-of-books " book. " (apply str (map book->string books)) ".")
      )
    )))

(defn books-by-author [author books]
  (filter boolean (map (fn [book] (if (contains? (get book :authors) author)
                    book
                    nil
                    ))
       books))
  )

(defn author-by-name [name authors]
  (let [results (filterv boolean
      (map (fn [author]
             (if (= name (get author :name))
                author
                nil
              )
            )authors
      ))]
    (if (> (count results) 0)
      (get results 0)
      nil
      )))


(defn living-authors [authors]
  (filter boolean
    (map
     (fn [author]
       (if (alive? author)
        author
        nil))
       authors)))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors))))
  )

(defn books-by-living-authors [books]
   (filter boolean
      (map (fn [book] (if (has-a-living-author? book)
                     book
                     nil
                     ))books)
           ))

; %________%
