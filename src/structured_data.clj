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
  :-)

(defn alive? [author]
  :-)

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

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
