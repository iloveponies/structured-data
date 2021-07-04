(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)  ))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)  ))

(defn cutify [v]
  (conj v "<3")  )

(defn spiff-destructuring [v]
  (let [[first _ third] v]
    (+ first third)  ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))  ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))  ))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (and (<= x1 px x2) (<= y1 py y2))  ))

(defn contains-rectangle? [outer inner]
  (let [[ilb irt] inner]
    (and (contains-point? outer ilb)
         (contains-point? outer irt)  )))

(defn title-length [book]
  (count (:title book))  )

(defn author-count [book]
  (count (:authors book))  )

(defn multiple-authors? [book]
  (< 1 (author-count book))  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))  )

(defn alive? [author]
  (not (contains? author :death-year))  )

(defn element-lengths [collection]
  (map count collection)  )

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection)  )

(defn titles [books]
  (map :title books)  )

(defn stars [n]
  (apply str (repeat n "*"))  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)  ))

(defn contains-duplicates? [a-seq]
  (>
   (count a-seq)
   (count (set a-seq))  ))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))  )

(defn has-author? [book author]
  (contains? (:authors book) author)  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))  )

(defn all-author-names [books]
  (set (map :name (authors books)))  )

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (cond
     death (str name " (" birth " - " death ")")
     birth (str name " (" birth " - )")
     :else name  )))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))  )

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))  ))

(defn books->string [books]
  (let [cnt (count books)
        bks (apply str (interpose ". " (map book->string books)))]
    (cond
     (== cnt 0) "No books."
     (== cnt 1) (str cnt " book. " bks ".")
     :else (str cnt " books. " bks ".")  )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)  )

(defn author-by-name [name authors]
  (first (filter (fn [auth] (= name (:name auth))) authors))  )

(defn living-authors [authors]
  (filter alive? authors)  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)  )

; %________%
