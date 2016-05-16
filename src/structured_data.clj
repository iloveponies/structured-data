(ns structured-data)

;Ex1 Change the function do-a-thing so that it uses let to give a name to the common expression (+ x x) in its body.
(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)
    ))

;Ex2 Write the function (spiff v) that takes a vector and returns the sum of the first and third elements of the vector. What happens when you pass in a vector that is too short?
(defn spiff [v]
  (if (> (count v) 2)
    (+ (get v 0) (get v 2))
    nil
    )
  )

;Ex3 Write the function (cutify v) that takes a vector as a parameter and adds "<3" to its end.
(defn cutify [v]
  (conj v "<3")
  )

;Ex4 Rewrite our earlier function spiff by destructuring its parameter. Call this new function spiff-destructuring.
(defn spiff-destructuring [v]
  (let [[x _ y] v]
    (if (and x y)
      (+ x y)
      nil
      )
    )
  )

(defn point [x y]
  [x y])

;Ex5 Write the functions (height rectangle) and (width rectangle) that return the height and width of the given rectangle. Use destructuring.
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))
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
