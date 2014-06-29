(ns structured-data)

(defn do-a-thing [x]
  (let [xtimes2 (+ x x )]
    (Math/pow xtimes2 xtimes2)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (cond
       (empty? v) nil
       (nil? third) first
     :else
       (+ first third)
     )))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  ;; destructure the first 3 elements into x1,x2,x3
  (let [[x1 x2 x3] v]
    (cond
       (empty? v) nil
       (nil? x3) x1
     :else
       (+ x1 x3)
    )))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  ;; return x2-x1
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
    ;; return y2-y1
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle)(width rectangle)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [px py] point]
     (and (<= x1 px x2)(<= y1 py y2))
     ))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (let [authors (:authors book)]
    (if (nil? authors)
      0
      (count (:authors book)))))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [old-authors (:authors book)
        new-authors (conj old-authors new-author)]
    (assoc book :authors new-authors)
    ))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  ;; If set contains same no of elements as seq, then no duplicates
  (not (== (count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (let [auth-set (set (:authors book))]
    (assoc book :authors auth-set)))

(defn has-author? [book author]
  (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors (map old-book->new-book books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        born (:birth-year author)
        died (if (alive? author)
                ""
                (:death-year author))
        dates (if (nil? born)
                ""
                (apply str " (" born " - " died ")")
                )]
    (str name dates)
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)]
    (cond
     (== n 0) "No books."
     (== n 1) (str "1 book. " (book->string (first books)) ".")
     :else
     (str n " books. " (apply str (interpose ". " (map book->string books))) ".")
     )
    )
)

(defn books-by-author [author books]
  (let [qbooks (filter (fn [bk] (has-author? bk author)) books)]
    (seq qbooks))
  )

(defn author-by-name [name authors]
  (let [auths (filter (fn [auth] (= name (:name auth))) authors)
        n (count auths)]
   (cond
    (== n 0)  nil
    (== n 1) (first auths)
    :else
    auths
    )
    )
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
