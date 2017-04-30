(ns structured-data)

(defn do-a-thing [x]
  (let  [xx (+ x x)]
     (float (reduce * (repeat xx xx)))
    )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)


(defn spiff-destructuring [v]
  (let [n[ (get v 0) (get v 2)] ]
  (+ (first n) (second n)))
)


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)
  )
)

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (cond (= (- y1 y2) (- x1 x2)) true
    :else false))
)


(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
  (let [h (height rectangle)]
  (let [w (width rectangle)]
  (* w h))))
)

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle]
   (let [[zx1 zy1] point]
   (cond
     (and (and (>= zx1 x1 ) (<= zx1 (+ x1 (width rectangle) ))  (>= zy1 y1 ) (<= zy1 (+ y1 (height rectangle) )))) true
    :else false)
     )
     )
)

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (let [p1 (point x1 y1)]
    (let [p2 (point x2 y2)]
    (cond
      (and (contains-point? outer p1) (contains-point? outer p2)) true
    :else false)
    )))
)



(defn title-length [book]
  (let [bookname (:title book)]
    (count bookname)
  )
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (cond
   (> 2 (count (:authors book))) false
    :else true)
)

(defn add-author [book new-author]
  (let [bookname (:title book)]
    (cond
      (= nil bookname)  (let [author (book :authors)]
   (assoc book :authors (conj author new-author)))
      :else
      (let [auth (book :authors)]
      (assoc book :authors (conj auth new-author)))))
)

(defn alive? [author]
  (cond
    (contains? author :death-year) false
    :else true)
)


(defn element-lengths [collection]
  (let [el-len (fn [x] (count x))]
  (map el-len collection))
)

(defn second-elements [collection]
  (let [el-seq(fn[x] (get x 1))]
  (map el-seq collection))
)

(defn titles [books]
  (let [thetitles [books]]
    (map :title books)
  )
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else  (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  (not= (count (distinct a-seq)) (count a-seq))
)

(defn old-book->new-book [book]
  (let [new-book book]
    (assoc new-book :authors (set (:authors book))))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
  (let [the-authors
    (fn [book] ( :authors book))]
    (set (apply concat (map the-authors books))))
)

(defn all-author-names [books]
  (let [the-names
    (fn [the-name] ( :name the-name))]
    (set (map the-names (authors books))))
)

(defn author->string [author]
  (let [the-name (:name author)]
     (let [birth (:birth-year author)]
       (let [death (:death-year author)]
        (cond
         (= 0(count (str birth))) (str the-name)
          :else  (str the-name " (" birth " - " death ")"))
       )
    )
)
)

(defn authors->string [authors]
  (let [all-authors
    (fn [author]
       (author->string author)
      )]
   (apply str (interpose ", " (map all-authors authors))))
)


(defn book->string [book]
  (str
    (:title book) ", written by "
    (authors->string (:authors book)))
)


(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
