(ns structured-data)

(defn do-a-thing [x]
  (let [+xx ( + x x)]
  (Math/pow +xx +xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [ [x y z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[bl_x bl_y] [tr_x tr_y]] rectangle]
    (-  tr_x bl_x)
  ))

(defn height [rectangle]
  (let [ [[bl_x bl_y] [tr_x tr_y]] rectangle]
    (-  tr_y bl_y)
  ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [ [[bl_x bl_y] [tr_x tr_y]] rectangle
         [x y] point]
  (and (<= bl_x x tr_x)
       (<= bl_y y tr_y))))

(defn contains-rectangle? [outer inner]
  (let [ [[obl_x obl_y] [otr_x otr_y]] outer
         [[ibl_x ibl_y] [itr_x itr_y]] inner]
    (and(<= obl_x ibl_x)
        (<= obl_y ibl_y)
        (>= otr_x itr_x)
        (>= otr_y itr_y)
    )))

(defn title-length [book]
  (count (:title book) ))

(defn author-count [book]
 (count (:authors book) ))


(defn multiple-authors? [book]
  (> (author-count book) 1))



(defn add-author [book new-author]
  (let [newAuthors (conj (:authors book)  new-author)]
   (assoc book :authors newAuthors)
    ))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [sec (fn[x](get x 1))]
    (map sec collection)
  ))

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)
    ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))


(defn contains-duplicates? [a-seq]
  (not(= (count a-seq) (count(set a-seq))))
  )

(defn old-book->new-book [book]
    (assoc book :authors (set(:authors book)))
   )

(defn has-author? [book author]
  (contains? (set (map :name (:authors book))) (:name author) ))

(defn authors [books]
  (apply clojure.set/union   (map :authors books)))

(defn all-author-names [books]
  (set (map :name ( authors books))))

(defn author->string [author]
  (let [name (:name author)
      b_year (:birth-year author)
      d_year (:death-year author)
      ]
     (if (boolean b_year)
                (str name " (" b_year " - " d_year ")" )
                (str name)

         )
  ))



(defn authors->string [authors]
  (apply str (interpose ", " (map  author->string authors))))

(defn book->string [book]
  (str (:title book ) ", written by "  (authors->string (:authors book)) ))

(defn books->string [books]
  (let [bcount (count books)]
  (cond
    (= 1 bcount ) (str bcount " book. " (apply str (map book->string books)) ".")
    (< 1 bcount ) (str bcount " books. " (apply str (map book->string books)) "." )
    :else                 "No books."
    )))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
