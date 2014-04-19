(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y)
    )
  )

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (cond
     (= x nil) nil
     (= y nil) nil
     :else (+ x y)
     )
    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (cond
     (= x nil) nil
     (= z nil) nil
     :else (+ x z)
     )
  )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[llx lly] [trx try]] rectangle]
    (- trx llx)
    )
  )

(defn height [rectangle]
  (let [[[llx lly] [trx try]] rectangle]
    (- try lly)
    )
  )

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
   (let [[[llx lly] [trx try]] rectangle
         [px py] point]
    (and
     (<= llx px trx)
     (<= lly py try))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[llp trp] inner]
    (and
     (contains-point? outer llp)
     (contains-point? outer trp)))
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
   (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))
    )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map
   (fn [x] (get x 1))
   collection
   )
  )

(defn titles [books]
 (map
   :title books)
  )

(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)
   )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [count-all (count a-seq)
        count-uniq (count (set a-seq))]
    (< count-uniq count-all)
    )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (apply clojure.set/union (map :authors books))))
  )

(defn author->string [author]
  (str
   (:name author)
   (let [birth (:birth-year author)
         death (:death-year author)]
     (cond
      (and (= birth nil) (= death nil)) ""
      (= death nil) (str " (" birth " - )")
      (= birth nil) (str " ( - " death ")")
      :else (str " (" birth " - " death ")")
      )
    )
   )
  )

(defn authors->string [authors]
   (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (str (:title book) ", " "written by " (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [count-books (count books)
        book-names (apply str  (interpose ". " (map book->string books)))]
         (cond
          (== count-books 0) "No books."
          (== count-books 1) (str "1 book. " book-names ".")
          :else (str count-books " books. " book-names ".")
          )
       )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)

  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))

  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
