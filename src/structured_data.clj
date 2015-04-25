(ns structured-data)

(defn do-a-thing [x]
  (let [ xx (+ x x ) ]
   (Math/pow xx xx)))

(defn spiff [v]
  (let [ aa (get v 0)
         bb (get v 2)]
   (+ aa bb)))

(defn cutify [v]
  (conj v "<3"))


(defn spiff-destructuring [v]
  (let [[x y z] v ]
    (+ x z )))


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
  (let [[[x1 y1] [x2 y2]] rectangle
        a (+ x1 y2)
        b (+ x2 y1)]
     (if (= a b) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        h (- y2 y1)
        w (- x2 x1)]
  (* h w)))

(defn contains-point? [rectangle point]
   (let [[[x1 y1] [x2 y2]] rectangle
         [p1 p2] point]
       (if (and (<= x1 p1 x2)(<= y1 p2 y2)) true false)
     ))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer
        [[ix1 iy1] [ix2 iy2]] inner]
        (contains-point? outer [ix1 iy1])
        (contains-point? outer [ix2 iy2])
    ))


(defn title-length [book]
  (count (:title book)) )

(defn author-count [book]
   (count (:authors book)))

(defn multiple-authors? [book]
  (if (< (author-count book) 2) false true) )


(defn add-author [book new-author]
(let [authors (:authors book)]
(assoc book :authors (conj authors new-author)
  )))

(defn alive? [author]
  (if (contains? author :death-year)false true))


(defn element-lengths [collection]
  (map count (seq collection)))


(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection))
   )

(defn titles [books]
  (let [titulars books]
    (map :title titulars))
   )

(defn monotonic? [a-seq]
(or (apply <= a-seq) (apply >= a-seq)
    ))


(defn stars [n]
(apply str (repeat n "*"))
  )



(defn toggle [a-set elem]
(cond
 (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem) ))


(defn contains-duplicates? [a-seq]
(let [setti (count (set a-seq))
      sequ (count a-seq)]
      (not= setti sequ)))

(defn old-book->new-book [book]
 (assoc book :authors
   (set (:authors book))
   ))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
   (set (map :name (authors books))))

(defn author->string [author]
   (let [nimi (:name author)]
    (cond
      (contains? author :death-year)(str nimi " ("(:birth-year author) " - "(:death-year author) ")")
      (contains? author :birth-year)(str nimi " (" (:birth-year author) " - )")
      :else nimi)
       ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))
         ))

(defn book->string [book]
  (let [kirja (:title book)
      kirjoittaja (authors->string (:authors book))]
      (str kirja ", written by " kirjoittaja)))

(defn books->string [books]
  :-)


(defn books-by-author [author books]
(filter (fn [x] (has-author? x author)) books
        ))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= name (:name x)))
                 authors)
         ))


(defn living-authors [authors]
    (filter alive? authors))

(defn has-a-living-author? [book]
  (let [elossa (-> book :authors living-authors)]
  (not (empty? elossa))
    ))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; ^____________________^
