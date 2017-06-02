(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))


(defn spiff [v]
  (+ (get v 0) (get v 2)))



(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let[[a b c] v]
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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (== (- y2 y1) (- x2 x1))
    )
  )


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
    )
  )


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x3 y3] point]
      (and
        (<= x1 x3 x2)
        (<= y1 y3 y2)
        )
      )
    )
  )


(defn contains-rectangle? [outer inner]
  (let [[xy1 xy2] inner]
    (and
      (contains-point? outer xy1)
      (contains-point? outer xy2)
      )
    )
  )



(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (< 1 (author-count book)))



(defn add-author [book new-author]
  (let [prevauthors (:authors book)]
    (assoc book :authors
      (conj prevauthors new-author)
    )
  )
  )


(defn alive? [author]
  (not (contains? author :death-year)))



(defn element-lengths [collection]
  (map count collection))



(defn second-elements [collection]
  (let
    [second (fn [sub] (get sub 1))]
      (map second collection
      )
  )
)


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))


(defn stars [n]
  (apply str
    (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )


(defn contains-duplicates? [a-seq]
  (not (==
    (count a-seq)
    (count (set a-seq))
    )
  )
)


(defn old-book->new-book [book]
  (let [newauthors (set (:authors book))]
    (assoc book :authors newauthors)))


(defn has-author? [book author]
  (contains? (:authors book) author))


(defn authors [books]
  (apply clojure.set/union
    (map :authors books)))


(defn all-author-names [books]
  (set
    (map :name (authors books))))




(defn author->string [author]
  (let [name (:name author)]
    (cond
      (contains? author :death-year)
        (let [date (str " (" (:birth-year author) " - " (:death-year author)")")]
          (str name date))
      (contains? author :birth-year)
        (let [date (str " (" (:birth-year author) " - )")]
          (str name date))
      :else (str name))))



(defn authors->string [authors]
  (apply str
    (interpose ", " (map author->string authors))))



(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))


(defn books->string [books]
  (let
    [n (count books)]
     (cond
       (== 0 n) "No books."
       (== 1 n)
         (str "1 book. " (book->string(get books 0)) ".")
       :else (let
               [booktext (apply str (interpose ". " (map book->string books)))]
                 (str n " books. " booktext ".")))))


(defn books-by-author [author books]
  (filter (fn [book] (contains? (:authors book) author)) books))


(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors)))



(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
