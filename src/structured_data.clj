(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)
        x2 (+ x x)]

 (Math/pow x2 xx))
)


(defn spiff [v]
  (cond
  (number? (get v 2))
  (let [a (get v 0)
        b (get v 2)]
    (+ a b))
   :else nil))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (if (> (count v) 2)
   (let [a (get v 0)
        b (get v 2)]
      (+ a b)
      )
     nil
))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 0)
        d(get b 0)]
        (- d c)
      )
)


  (defn height [rectangle]
  (let [a (get rectangle 0)
        b (get rectangle 1)
        c (get a 1)
        d(get b 1)]
    (- d c)
      )
)

(defn square? [rectangle]
  :-)

(defn area [rectangle]
  :-)

(defn contains-point? [rectangle point]
  :-)

(defn contains-rectangle? [outer inner]
  :-)

(defn title-length [book]
  :-)

(defn author-count [book]
  :-)

(defn multiple-authors? [book]
  :-)

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
