(ns structured-data)

Math/PI

(Math/pow 2 3)

(defn do-a-thing [x]
    (let [y (+ x x)]
        (Math/pow y y)))

(do-a-thing 2)

(defn spiff [v]
    (if (< (count v) 2)
        nil
        (+ (v 0) (v 1))))

(spiff [1 2 3])
(spiff [1 2 3 4 5 6])
(spiff [1 2])
(spiff [])

(defn cutify [v]
    (conj v "<3"))

(cutify [])
(cutify [1 2 3])
(cutify ["a" "b"])



(defn spiff-destructuring [v]
    v)

(defn point [x y]
    [x y])

(defn rectangle [bottom-left top-right]
    [bottom-left top-right])

(defn width [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
        (Math/abs (- x2 x1))))

(defn height [rectangle]
    (let [[[x1 y1] [x2 y2]] rectangle]
        (Math/abs (- y2 y1))))


(height (rectangle [1 1] [5 1]))
(height (rectangle [1 1] [5 5]))
(height (rectangle [0 0] [2 3]))

(width (rectangle [1 1] [5 1]))
(width (rectangle [1 1] [1 1]))
(width (rectangle [3 1] [10 4]))

(defn square? [rectangle]
    (==
        (width rectangle)
        (height rectangle)))


(square? (rectangle [1 1] [2 2]))
(square? (rectangle [1 1] [2 3]))
(square? (rectangle [1 1] [1 1]))
(square? (rectangle [3 2] [1 0]))
(square? (rectangle [3 2] [1 1]))


(defn area [rectangle]
    (*
        (width rectangle)
        (height rectangle)))


(area (rectangle [1 1] [5 1]))
(area (rectangle [0 0] [1 1]))
(area (rectangle [0 0] [4 3]))
(area (rectangle [3 1] [10 4]))

(defn x [point]
    (point 0))

(defn y [point]
    (point 1))


(defn ll [rect]
(rect 0))

(defn ur [rect]
    (rect 1))

(defn point-less-than? [point1 point2]
    (and
        (< (x point1) (x point2))
        (< (y point1) (y point2))))

  (point-less-than? [2 1] [2 2])

(defn contains-point? [rectangle point]
    (and
        (point-less-than? (ll rectangle) point)
        (point-less-than?  point (ur rectangle))))

(contains-point? (rectangle [0 0] [2 2]) (point 2 1))


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
