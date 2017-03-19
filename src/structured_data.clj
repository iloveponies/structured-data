(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [sum (+ (get v 0) (get v 2))]
    sum))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a b c]]
  (+ a c))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs(- x1 x2))
)

(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs(- y1 y2))
)

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false
  ))

(defn area [rectangle]
  (* (width rectangle)(height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle
         [p1 p2] point
         ]
      (cond
    (< p1 (Math/min x1 x2)) false
    (> p1 (Math/max x1 x2)) false
    (< p2 (Math/min y1 y2)) false
    (> p2 (Math/max y1 y2)) false
    :else true
    )))


(defn contains-rectangle? [outer inner]
   (let [ [[x1 y1] [x2 y2]] outer
         [[bx1 by1] [bx2 by2]] inner
          minX (Math/min x1 x2)
          minY (Math/min y1 y2)
          minbX (Math/min bx1 bx2)
          minbY (Math/min by1 by2)
          maxX (Math/max x1 x2)
          maxY (Math/max y1 y2)
          maxbX (Math/max bx1 bx2)
          maxbY (Math/max by1 by2)
         ]
     (and (<= minX minbX maxbX maxX)(<= minY minbY maxbY maxY)
  )))


(defn title-length [book]
  (count (get book :title)))



(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))


(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
    (assoc book :authors authors)
    ))

(defn alive? [author]
  (not(contains? author :death-year))
  )

(defn get-length [x]
  (count x))

(defn element-lengths [collection]
 (map get-length collection))

(defn second-elements [collection]
  (let [sec (fn [x] (first (rest x)))]
    (map sec collection)
  ))


(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  ((if (contains? a-set elem) disj conj) a-set elem))

(defn contains-duplicates? [seq]
  (not (== (count seq) (count (set seq)))))


(defn old-book->new-book [book]
  (let [authors #(book :authors)]
    (assoc book :authors (set (book :authors)))))


(defn has-author? [book author]
  (let [authors (book :authors)]
    (< 0 (count(get authors author)))
  ))

(defn authors [books]
    (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (let [all-authors (authors books)]
    (set (map :name all-authors))
  ))

(defn author->string [author]
  (let [name (author :name)
        death (author :death-year)
        born (author :birth-year)]
    (if(= nil born) (str name death born) (if(= nil death) (str name " (" born " - )") (str name " (" born " - " death ")"))
    )))

(defn authors->string [authors]
  (apply str ( interpose ", "(map author->string authors)))
  )

(defn book->string [book]
  (apply str (book :title) ", written by " (authors->string (book :authors) ))
  )

(defn books->string [books]
  (let [n (count books)]
  (cond
    (= n 0) "No books."
    (= n 1) (str "1 book. " (book->string (first books)) ".")
    :else (str(apply str n " books. " (interpose ". " (map book->string books)))"."))))


(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))


(defn author-by-name [name authors]
   (first (filter (fn [x] (= name (:name x))) authors)))


(defn living-authors [authors]
  (filter (fn [x] (= nil (:death-year x))) authors))

(defn has-a-living-author? [book]
  (not(empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )


; %________%
